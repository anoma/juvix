module Commands.Markdown where

import Commands.Base
import Commands.Markdown.Options
import Data.Text.IO qualified as Text
import Juvix.Compiler.Backend.Markdown.Translation.FromTyped.Source
import Juvix.Compiler.Backend.Markdown.Translation.FromTyped.Source qualified as MK
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language qualified as Concrete
import Juvix.Compiler.Concrete.Pretty qualified as Concrete
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Extra.Assets (writeAssets)

runCommand ::
  (Members AppEffects r) =>
  MarkdownOptions ->
  Sem r ()
runCommand opts = do
  let inputFile = opts ^. markdownInputFile
  scopedM <- runPipelineNoOptions inputFile upToScopingEntry
  let m = scopedM ^. Scoper.resultModule
  outputDir <- fromAppPathDir (opts ^. markdownOutputDir)
  let res =
        MK.fromJuvixMarkdown'
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
  case res of
    Left err -> exitJuvixError (JuvixError err)
    Right md
      | opts ^. markdownStdout -> liftIO . putStrLn $ md
      | otherwise -> do
          ensureDir outputDir
          when (opts ^. markdownWriteAssets) $
            liftIO $
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

          liftIO $ Text.writeFile (toFilePath absPath) md
