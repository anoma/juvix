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
    _processJuvixBlocksArgsExt :: Text,
    _processJuvixBlocksArgsStripPrefix :: Text,
    _processJuvixBlocksArgsFolderStructure :: Bool,
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
  forall r.
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
            HtmlRender._htmlOptionsExt = opts ^. processJuvixBlocksArgsExt,
            HtmlRender._htmlOptionsStripPrefix = opts ^. processJuvixBlocksArgsStripPrefix,
            HtmlRender._htmlOptionsOutputDir = opts ^. processJuvixBlocksArgsOutputDir,
            HtmlRender._htmlOptionsFolderStructure = opts ^. processJuvixBlocksArgsFolderStructure
          }

      m :: Concrete.Module 'Concrete.Scoped 'Concrete.ModuleTop
      m = opts ^. processJuvixBlocksArgsModule

      fname :: Path Abs File
      fname = getLoc m ^. intervalFile

  let err :: forall a. Sem r a
      err =
        throw $
          ErrInternalNoMarkdownInfo
            NoMarkdownInfoError
              { _noMarkdownInfoFilepath = fname
              }

  mkInfo :: Concrete.MarkdownInfo <- maybe err return (m ^. Concrete.moduleMarkdownInfo)
  let mk :: Mk = mkInfo ^. Concrete.markdownInfo
      sepr :: [Int] = mkInfo ^. Concrete.markdownInfoBlockLengths

  when (nullMk mk || null sepr)
    . throw
    $ ErrNoJuvixCodeBlocks
      NoJuvixCodeBlocksError
        { _noJuvixCodeBlocksErrorFilepath = fname
        }

  let iniState =
        ProcessingState
          { _processingStateMk = mk,
            _processingStateFirstBlock = True,
            _processingStateStmtsSeparation = sepr,
            _processingStateStmts = indModuleFilter $ m ^. Concrete.moduleBody
          }
  r :: Mk <-
    evalState iniState
      . runReader htmlOptions
      . runReader opts
      $ go fname
  return (MK.toPlainText r)

htmlSemicolon :: Html
htmlSemicolon = Html.span ! HtmlRender.juClass HtmlRender.JuDelimiter $ ";"

go ::
  forall r.
  ( Members
      '[ Reader HtmlRender.HtmlOptions,
         Reader ProcessJuvixBlocksArgs,
         State ProcessingState,
         Error MarkdownBackendError
       ]
      r
  ) =>
  Path Abs File ->
  Sem r Mk
go fname = do
  stmts <- gets @ProcessingState (^. processingStateStmts)
  sepr <- gets @ProcessingState (^. processingStateStmtsSeparation)
  mk <- gets @ProcessingState (^. processingStateMk)
  case sepr of
    [] -> return mk
    n : ns -> do
      case mk of
        MkNull -> return mk
        MkTextBlock _ -> return mk
        MkConcat l r -> do
          modify (set processingStateMk l)
          lS <- go fname
          modify (set processingStateMk r)
          MkConcat lS <$> go fname
        MkJuvixCodeBlock j -> do
          opts <- case parseJuvixBlockOptions fname (j ^. juvixCodeBlockOptions) of
            Left e ->
              throw
                ( ErrInvalidCodeBlockAttribtues
                    (InvalidCodeBlockAttributesError e)
                )
            Right o -> return o

          m <-
            asks @ProcessJuvixBlocksArgs
              (^. processJuvixBlocksArgsModule)

          isFirstBlock <- gets @ProcessingState (^. processingStateFirstBlock)

          stmts' <-
            let blockStmts = take n stmts
             in case opts of
                  MkJuvixBlockOptionsExtractModule o ->
                    checkExtractModule j (o ^. juvixBlockOptionsExtractModuleDrop) blockStmts
                  _ -> return blockStmts

          htmlStatements :: [Html] <-
            mapM (\stm -> goRender stm <> pure htmlSemicolon) stmts'

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

          let _processingStateMk = case opts of
                MkJuvixBlockOptionsHide -> MkNull
                _ ->
                  MkTextBlock
                    TextBlock
                      { _textBlock = Text.replace "\n" "<br/>" resHtml,
                        _textBlockLoc = j ^. juvixCodeBlockLoc
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
      where
        checkExtractModule :: JuvixCodeBlock -> Int -> [Concrete.Statement 'Concrete.Scoped] -> Sem r [Concrete.Statement 'Concrete.Scoped]
        checkExtractModule j dropNum xs = case xs of
          [Concrete.StatementModule m] -> do
            return (drop dropNum (indModuleFilter (m ^. Concrete.moduleBody)))
          _ ->
            throw
              ( ErrInvalidExtractModuleBlock
                  ( InvalidExtractModuleBlockError
                      { _invalidExtractModuleBlockErrorPath = fname,
                        _invalidExtractModuleBlockErrorLoc = j ^. juvixCodeBlockLoc
                      }
                  )
              )

goRender ::
  (Concrete.PrettyPrint c, Members '[Reader HtmlRender.HtmlOptions, Reader ProcessJuvixBlocksArgs] r) =>
  c ->
  Sem r Html
goRender xs = do
  concreteOpts <- asks @ProcessJuvixBlocksArgs (^. processJuvixBlocksArgsConcreteOpts)
  HtmlRender.ppCodeHtml concreteOpts xs

indModuleFilter :: forall s. [Concrete.Statement s] -> [Concrete.Statement s]
indModuleFilter = filter Concrete.statementShouldBePrinted
