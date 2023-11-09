module Juvix.Compiler.Backend.Markdown.Translation.FromTyped.Source where

import Commonmark qualified as MK
import Data.Text qualified as Text
import Data.Text.Lazy (toStrict)
import Juvix.Compiler.Backend.Html.Data.Options qualified as HtmlRender
import Juvix.Compiler.Backend.Html.Translation.FromTyped.Source qualified as HtmlRender
import Juvix.Compiler.Backend.Markdown.Data.Types
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
    _processingStateFlag :: Bool,
    _processingStateSep :: [Int],
    _processingStateStmts :: [Concrete.Statement 'Concrete.Scoped]
  }

makeLenses ''ProcessJuvixBlocksArgs
makeLenses ''ProcessingState

fromJuvixMarkdown' :: ProcessJuvixBlocksArgs -> Text
fromJuvixMarkdown' = run . fromJuvixMarkdown

fromJuvixMarkdown ::
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

  case (m ^. Concrete.moduleMarkdown, m ^. Concrete.moduleMarkdownSeparation) of
    (Just mk, Just sepr) -> do
      let st =
            ProcessingState
              { _processingStateMk = mk,
                _processingStateFlag = True,
                _processingStateSep = sepr,
                _processingStateStmts = indModuleFilter $ m ^. Concrete.moduleBody
              }
      (_, r) <- runState st . runReader htmlOptions . runReader opts $ go
      return $ MK.toPlainText r
    (Nothing, _) -> error "This module has no Markdown"
    (_, _) -> error "This Markdown file has no Juvix code blocks"

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
  sepr <- gets @ProcessingState (^. processingStateSep)
  stmts <- gets @ProcessingState (^. processingStateStmts)
  mk <- gets @ProcessingState (^. processingStateMk)
  case (sepr, stmts) of
    ([], _) -> return mk
    ((n : ns), _) -> do
      case mk of
        MkNull -> return mk
        MkTextBlock _ -> return mk
        MkConcat l r -> do
          modify (over processingStateMk (const l))
          lS <- go
          modify (over processingStateMk (const r))
          MkConcat lS <$> go
        MkJuvixCodeBlock j -> do
          m <-
            asks @ProcessJuvixBlocksArgs
              (^. processJuvixBlocksArgsModule)

          f <- gets @ProcessingState (^. processingStateFlag)

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
                if f
                  then do
                    let m' = set Concrete.moduleBody stmts' m
                    goRender m'
                  else
                    return $
                      Html.preEscapedText $
                        Text.intercalate "\n\n" $
                          map (toStrict . Html.renderHtml) htmlStatements
          let mock =
                if j ^. juvixCodeBlockOptions . mkJuvixBlockOptionsHide
                  then MkNull
                  else
                    MkTextBlock
                      TextBlock
                        { _textBlock = resHtml,
                          _textBlockInterval = j ^. juvixCodeBlockInterval
                        }

          modify @ProcessingState $ \s ->
            s
              { _processingStateMk = mock,
                _processingStateFlag = False,
                _processingStateSep = ns,
                _processingStateStmts = drop n stmts
              }
          return mock

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
