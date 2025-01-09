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

-- data Mode
--   = Project (Path Abs Dir)
--   | SingleFile (Path Abs File)

-- getMode :: forall r. (Members '[App, EmbedIO] r) => Maybe (AppPath FileOrDir) -> Sem r Mode
-- getMode mf = runFailDefaultM projectMode $ do
--   optFile <- failMaybe mf
--   either SingleFile Project <$> fromAppPathFileOrDir optFile
--   where
--     projectMode :: Sem r Mode
--     projectMode = Project . (^. rootRootDir) <$> askRoot

-- runCommand ::
--   forall r.
--   (Members AppEffects r) =>
--   MarkdownOptions ->
--   Sem r ()
-- runCommand opts = silenceProgressLog . runPipelineOptions . runPipelineSetup $ do
--   mode :: Mode <- getMode (opts ^. markdownInputFile)
--   let inputFile = case mode of
--         Project {} -> Nothing
--         SingleFile f ->
--           Just
--             AppPath
--               { _pathPath = preFileFromAbs f,
--                 _pathIsInput = True
--               }
--   let shouldRecurse :: ImportNode -> Bool
--       shouldRecurse node = case mode of
--         Project p -> p == node ^. importNodePackageRoot
--         SingleFile {} -> False
--   (scopedM, others) :: (Scoper.ScoperResult, [Scoper.ScoperResult]) <-
--     runPipelineEither () inputFile (processRecursivelyUpTo shouldRecurse upToScopingEntry)
--       >>= fmap ((^. pipelineResult) . snd) . getRight
--   mapM_ goScoperResult (scopedM : others)
--   where
--     goScoperResult :: Scoper.ScoperResult -> Sem r ()
--     goScoperResult scopedM = do
--       let m :: Module 'Scoped 'ModuleTop = scopedM ^. Scoper.resultModule
--       outputDir <- fromAppPathDir (opts ^. markdownOutputDir)
--       let args =
--             ProcessJuvixBlocksArgs
--               { _processJuvixBlocksArgsConcreteOpts = Concrete.defaultOptions,
--                 _processJuvixBlocksArgsUrlPrefix = opts ^. markdownUrlPrefix,
--                 _processJuvixBlocksArgsIdPrefix =
--                   opts ^. markdownIdPrefix,
--                 _processJuvixBlocksArgsNoPath =
--                   opts ^. markdownNoPath,
--                 _processJuvixBlocksArgsExt =
--                   opts ^. markdownExt,
--                 _processJuvixBlocksArgsStripPrefix =
--                   opts ^. markdownStripPrefix,
--                 _processJuvixBlocksArgsComments = Scoper.getScoperResultComments scopedM,
--                 _processJuvixBlocksArgsModule = m,
--                 _processJuvixBlocksArgsOutputDir = outputDir,
--                 _processJuvixBlocksArgsFolderStructure =
--                   opts ^. markdownFolderStructure
--               }

--       md :: Text <- runAppError @MarkdownBackendError (MK.fromJuvixMarkdown args)
--       if
--           | opts ^. markdownStdout -> putStrLn md
--           | otherwise -> do
--               ensureDir outputDir
--               when (opts ^. markdownWriteAssets) $
--                 writeAssets outputDir

--               let mdFile :: Path Rel File
--                   mdFile =
--                     relFile
--                       ( Concrete.topModulePathToDottedPath
--                           (m ^. Concrete.modulePath . S.nameConcrete)
--                           <.> markdownFileExt
--                       )
--                   absPath :: Path Abs File
--                   absPath = outputDir <//> mdFile

--               writeFileEnsureLn absPath md

runCommand ::
  forall r.
  (Members AppEffects r) =>
  MarkdownOptions ->
  Sem r ()
runCommand opts = silenceProgressLog . runPipelineOptions . runPipelineSetup $ do
  -- mode :: Mode <- getMode (opts ^. markdownInputFile)
  -- let inputFile = case mode of
  --       Project {} -> Nothing
  --       SingleFile f ->
  --         Just
  --           AppPath
  --             { _pathPath = preFileFromAbs f,
  --               _pathIsInput = True
  --             }
  res :: [ProcessedNode ScoperResult] <- processProjectUpToScoping
  forM_ res (goScoperResult opts . (^. processedNodeData))

goScoperResult :: (Members AppEffects r) => MarkdownOptions -> Scoper.ScoperResult -> Sem r ()
goScoperResult opts scopedM = do
  let m :: Module 'Scoped 'ModuleTop = scopedM ^. Scoper.resultModule
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
