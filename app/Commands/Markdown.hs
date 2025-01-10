module Commands.Markdown (runCommand) where

import Commands.Base
import Commands.Markdown.Options
import Juvix.Compiler.Backend.Markdown.Error
import Juvix.Compiler.Backend.Markdown.Translation.FromTyped.Source
import Juvix.Compiler.Backend.Markdown.Translation.FromTyped.Source qualified as MK
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language as Concrete
import Juvix.Compiler.Concrete.Pretty as Concrete
import Juvix.Compiler.Concrete.Print.Base qualified as Concrete
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context
import Juvix.Data.CodeAnn
import Juvix.Extra.Assets (writeAssets)

runCommand ::
  forall r.
  (Members AppEffects r) =>
  MarkdownOptions ->
  Sem r ()
runCommand opts = runPipelineOptions . runPipelineSetup $ do
  res :: [ProcessedNode ScoperResult] <- processProjectUpToScoping
  forM_ res (goScoperResult opts . (^. processedNodeData))

goScoperResult :: (Members AppEffects r) => MarkdownOptions -> Scoper.ScoperResult -> Sem r ()
goScoperResult opts scopedM = do
  let m :: Module 'Scoped 'ModuleTop = scopedM ^. Scoper.resultModule
  if
      | isNothing (m ^. moduleMarkdownInfo) ->
          logInfo (mkAnsiText @(Doc CodeAnn) ("Skipping" <+> Concrete.docNoCommentsDefault (m ^. modulePath)))
      | otherwise -> do
          outputDir <- fromAppPathDir (opts ^. markdownOutputDir)
          logProgress (mkAnsiText @(Doc CodeAnn) ("Processing" <+> Concrete.docNoCommentsDefault (m ^. modulePath)))
          let args =
                ProcessJuvixBlocksArgs
                  { _processJuvixBlocksArgsConcreteOpts = Concrete.defaultOptions,
                    _processJuvixBlocksArgsUrlPrefix = opts ^. markdownUrlPrefix,
                    _processJuvixBlocksArgsIdPrefix =
                      opts ^. markdownIdPrefix,
                    _processJuvixBlocksArgsNoPath =
                      opts ^. markdownNoPath,
                    _processJuvixBlocksArgsExt =
                      opts ^. markdownExt,
                    _processJuvixBlocksArgsStripPrefix =
                      opts ^. markdownStripPrefix,
                    _processJuvixBlocksArgsComments = Scoper.getScoperResultComments scopedM,
                    _processJuvixBlocksArgsModule = m,
                    _processJuvixBlocksArgsOutputDir = outputDir,
                    _processJuvixBlocksArgsFolderStructure =
                      opts ^. markdownFolderStructure
                  }

          md :: Text <- runAppError @MarkdownBackendError (MK.fromJuvixMarkdown args)
          if
              | opts ^. markdownStdout -> putStrLn md
              | otherwise -> do
                  ensureDir outputDir
                  when (opts ^. markdownWriteAssets) $
                    writeAssets outputDir

                  let mdFile :: Path Rel File
                      mdFile =
                        relFile
                          ( Concrete.topModulePathToDottedPath
                              (m ^. Concrete.modulePath . S.nameConcrete)
                              <.> markdownFileExt
                          )
                      absPath :: Path Abs File
                      absPath = outputDir <//> mdFile

                  writeFileEnsureLn absPath md
