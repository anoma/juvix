module Juvix.Compiler.Backend.Markdown.Translation.FromTyped.Source where

import Commonmark qualified as MK
import Data.Text qualified as Text
import Data.Text.Lazy (toStrict)
import Juvix.Compiler.Backend.Html.Data.Options qualified as HtmlRender
import Juvix.Compiler.Backend.Html.Translation.FromTyped.Source qualified as HtmlRender
import Juvix.Compiler.Backend.Markdown.Data.Types
import Juvix.Compiler.Backend.Markdown.Error
import Juvix.Compiler.Concrete.Language qualified as Concrete
import Juvix.Compiler.Concrete.Pretty qualified as Concrete
import Juvix.Prelude
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text qualified as Html
import Text.Blaze.Html5 as Html hiding (map)
import Text.Blaze.Html5.Attributes qualified as Attr

data ProcessJuvixBlocksArgs = ProcessJuvixBlocksArgs
  { _processJuvixBlocksArgsConcreteOpts :: Concrete.Options,
    _processJuvixBlocksArgsUrlPrefix :: Text,
    _processJuvixBlocksArgsIdPrefix :: Text,
    _processJuvixBlocksArgsNoPath :: Bool,
    _processJuvixBlocksArgsComments :: Comments,
    _processJuvixBlocksArgsOutputDir :: Path Abs Dir,
    _processJuvixBlocksArgsModule :: Concrete.Module 'Concrete.Scoped 'Concrete.ModuleTop
  }

data ProcessingState = ProcessingState
  { _processingStateMk :: Mk,
    _processingStateFirstBlock :: Bool,
    _processingStateStmtsSeparation :: [Int],
    _processingStateStmts :: [Concrete.Statement 'Concrete.Scoped]
  }

makeLenses ''ProcessJuvixBlocksArgs
makeLenses ''ProcessingState

fromJuvixMarkdown' :: ProcessJuvixBlocksArgs -> Either MarkdownBackendError Text
fromJuvixMarkdown' = run . runError . fromJuvixMarkdown

fromJuvixMarkdown ::
  (Members '[Error MarkdownBackendError] r) =>
  ProcessJuvixBlocksArgs ->
  Sem r Text
fromJuvixMarkdown opts = do
  let htmlOptions :: HtmlRender.HtmlOptions
      htmlOptions =
        HtmlRender.defaultHtmlOptions
          { HtmlRender._htmlOptionsKind = HtmlRender.HtmlOnly,
            HtmlRender._htmlOptionsAssetsPrefix = opts ^. processJuvixBlocksArgsUrlPrefix,
            HtmlRender._htmlOptionsUrlPrefix = opts ^. processJuvixBlocksArgsUrlPrefix,
            HtmlRender._htmlOptionsIdPrefix = opts ^. processJuvixBlocksArgsIdPrefix,
            HtmlRender._htmlOptionsNoPath = opts ^. processJuvixBlocksArgsNoPath,
            HtmlRender._htmlOptionsOutputDir = opts ^. processJuvixBlocksArgsOutputDir
          }

      m :: Concrete.Module 'Concrete.Scoped 'Concrete.ModuleTop
      m = opts ^. processJuvixBlocksArgsModule

      fname :: Path Abs File
      fname = getLoc m ^. intervalFile

  case m ^. Concrete.moduleMarkdownInfo of
    Just mkInfo -> do
      let mk :: Mk = mkInfo ^. Concrete.markdownInfo
          sepr :: [Int] = mkInfo ^. Concrete.markdownInfoBlockLengths

      when (nullMk mk || null sepr) $
        throw
          ( ErrNoJuvixCodeBlocks
              NoJuvixCodeBlocksError
                { _noJuvixCodeBlocksErrorFilepath = fname
                }
          )

      let st =
            ProcessingState
              { _processingStateMk = mk,
                _processingStateFirstBlock = True,
                _processingStateStmtsSeparation = sepr,
                _processingStateStmts = indModuleFilter $ m ^. Concrete.moduleBody
              }
      (_, r) <- runState st . runReader htmlOptions . runReader opts $ go
      return $ MK.toPlainText r
    Nothing ->
      throw
        ( ErrInternalNoMarkdownInfo
            NoMarkdownInfoError
              { _noMarkdownInfoFilepath = fname
              }
        )

htmlSemicolon :: Html
htmlSemicolon = Html.span ! HtmlRender.juColor HtmlRender.JuDelimiter $ ";"

go ::
  forall r.
  ( Members
      '[ Reader HtmlRender.HtmlOptions,
         Reader ProcessJuvixBlocksArgs,
         State ProcessingState
       ]
      r
  ) =>
  Sem r Mk
go = do
  stmts <- gets @ProcessingState (^. processingStateStmts)
  sepr <- gets @ProcessingState (^. processingStateStmtsSeparation)
  mk <- gets @ProcessingState (^. processingStateMk)
  case (sepr, stmts) of
    ([], _) -> return mk
    ((n : ns), _) -> do
      case mk of
        MkNull -> return mk
        MkTextBlock _ -> return mk
        MkConcat l r -> do
          modify (set processingStateMk l)
          lS <- go
          modify (set processingStateMk r)
          MkConcat lS <$> go
        MkJuvixCodeBlock j -> do
          m <-
            asks @ProcessJuvixBlocksArgs
              (^. processJuvixBlocksArgsModule)

          isFirstBlock <- gets @ProcessingState (^. processingStateFirstBlock)

          let stmts' = take n stmts

          htmlStatements :: [Html] <-
            mapM (\s -> goRender s <> pure htmlSemicolon) stmts'

          resHtml <-
            toStrict
              . Html.renderHtml
              . (pre ! Attr.class_ "highlight")
              . (code ! Attr.class_ "juvix")
              . (pre ! Attr.class_ "src-content")
              <$> do
                if isFirstBlock
                  then do
                    let m' = set Concrete.moduleBody stmts' m
                    goRender m'
                  else
                    return $
                      Html.preEscapedText $
                        Text.intercalate "\n\n" $
                          map (toStrict . Html.renderHtml) htmlStatements

          let _processingStateMk =
                if j ^. juvixCodeBlockOptions . mkJuvixBlockOptionsHide
                  then MkNull
                  else
                    MkTextBlock
                      TextBlock
                        { _textBlock = Text.replace "\n" "<br/>" resHtml,
                          _textBlockInterval = j ^. juvixCodeBlockInterval
                        }
          let newState =
                ProcessingState
                  { _processingStateFirstBlock = False,
                    _processingStateStmtsSeparation = ns,
                    _processingStateStmts = drop n stmts,
                    ..
                  }
          modify @ProcessingState $ const newState
          return _processingStateMk

goRender :: (Concrete.PrettyPrint a, Members '[Reader HtmlRender.HtmlOptions, Reader ProcessJuvixBlocksArgs] r) => a -> Sem r Html
goRender xs = do
  concreteOpts <- asks @ProcessJuvixBlocksArgs (^. processJuvixBlocksArgsConcreteOpts)
  HtmlRender.ppCodeHtml concreteOpts xs

indModuleFilter :: [Concrete.Statement s] -> [Concrete.Statement s]
indModuleFilter =
  filter
    ( \case
        Concrete.StatementSyntax _ -> True
        Concrete.StatementFunctionDef _ -> True
        Concrete.StatementImport _ -> True
        Concrete.StatementInductive _ -> True
        Concrete.StatementModule o -> not (o ^. Concrete.moduleInductive)
        Concrete.StatementOpenModule _ -> True
        Concrete.StatementAxiom _ -> True
        Concrete.StatementProjectionDef _ -> True
    )
