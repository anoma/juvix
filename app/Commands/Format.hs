module Commands.Format where

import Commands.Base
import Commands.Format.Options
-- import Juvix.Compiler.Pipeline.Loader.PathResolver.ImportTree.Base

import Data.Text qualified as Text
import Juvix.Compiler.Pipeline.Driver (processModule)
import Juvix.Compiler.Pipeline.Loader.PathResolver.ImportTree.Base
import Juvix.Compiler.Pipeline.ModuleInfoCache
import Juvix.Compiler.Store.Language (ModuleInfo)
import Juvix.Formatter

data FormatNoEditRenderMode
  = ReformattedFile Text
  | InputPath (Path Abs File)
  | Silent

data FormatRenderMode
  = EditInPlace FormattedFileInfo
  | NoEdit FormatNoEditRenderMode

data FormatTarget
  = TargetFile (Path Abs File)
  | TargetProject
  | TargetStdin

isTargetProject :: FormatTarget -> Bool
isTargetProject = \case
  TargetProject {} -> True
  _ -> False

targetFromOptions :: (Members '[EmbedIO, App] r) => FormatOptions -> Sem r FormatTarget
targetFromOptions opts = do
  globalOpts <- askGlobalOptions
  let isStdin = globalOpts ^. globalStdin
  f <- mapM fromAppPathFileOrDir (opts ^. formatInput)
  case f of
    Just (Left p) -> return (TargetFile p)
    Just Right {} -> return TargetProject
    Nothing -> do
      isPackageGlobal <- askPackageGlobal
      if
          | isStdin -> return TargetStdin
          | not isPackageGlobal -> return TargetProject
          | otherwise -> do
              exitFailMsg $
                Text.unlines
                  [ "juvix format error: either 'JUVIX_FILE_OR_PROJECT' or '--stdin' option must be specified",
                    "Use the --help option to display more usage information."
                  ]

-- | Formats the project on the root
formatProjectNew ::
  forall r.
  (Members '[App, EmbedIO, TaggedLock, Files, Output FormattedFileInfo] r) =>
  Sem r FormatResult
formatProjectNew = runPipelineOptions . runPipelineSetup $ do
  pkg <- askPackage
  nodes <- toList <$> asks (^. importTreeNodes)
  res :: [(ImportNode, PipelineResult ModuleInfo)] <- forM nodes $ \node -> do
    res <- mkEntryIndex node >>= processModule
    return (node, res)
  res' :: [(ImportNode, SourceCode)] <- runReader pkg . forM res $ \(node, nfo) -> do
    src <- formatModuleInfo node nfo
    return (node, src)
  formatProject res'

runCommand :: forall r. (Members '[EmbedIO, App, TaggedLock, Files] r) => FormatOptions -> Sem r ()
runCommand opts = do
  target <- targetFromOptions opts
  runOutputSem (renderFormattedOutput target opts) . runScopeFileApp $ do
    res <- case target of
      TargetFile p -> format p
      TargetProject -> formatProjectNew
      TargetStdin -> do
        entry <- getEntryPointStdin
        runReader entry formatStdin
    case res of
      FormatResultFail -> exitFailure
      FormatResultNotFormatted ->
        {- use exit code 1 for
         * unformatted files when using --check
         * when running the formatter on a Juvix project
        -}
        when (opts ^. formatCheck || isTargetProject target) exitFailure
      FormatResultOK -> pure ()

renderModeFromOptions :: FormatTarget -> FormatOptions -> FormattedFileInfo -> FormatRenderMode
renderModeFromOptions target opts formattedInfo
  | opts ^. formatInPlace = whenContentsModified (EditInPlace formattedInfo)
  | opts ^. formatCheck = NoEdit Silent
  | otherwise = case target of
      TargetFile {} -> NoEdit (ReformattedFile (formattedInfo ^. formattedFileInfoContents))
      TargetProject {} -> whenContentsModified (NoEdit (InputPath (formattedInfo ^. formattedFileInfoPath)))
      TargetStdin -> NoEdit (ReformattedFile (formattedInfo ^. formattedFileInfoContents))
  where
    whenContentsModified :: FormatRenderMode -> FormatRenderMode
    whenContentsModified res
      | formattedInfo ^. formattedFileInfoContentsModified = res
      | otherwise = NoEdit Silent

renderFormattedOutput :: forall r. (Members '[EmbedIO, App, Files] r) => FormatTarget -> FormatOptions -> FormattedFileInfo -> Sem r ()
renderFormattedOutput target opts fInfo = do
  let renderMode = renderModeFromOptions target opts fInfo
  outputResult renderMode
  where
    outputResult :: FormatRenderMode -> Sem r ()
    outputResult = \case
      EditInPlace i@FormattedFileInfo {..} ->
        runTempFileIO
          . restoreFileOnError _formattedFileInfoPath
          $ writeFileEnsureLn' _formattedFileInfoPath (i ^. formattedFileInfoContents)
      NoEdit m -> case m of
        ReformattedFile ts -> renderStdOut ts
        InputPath p -> say (pack (toFilePath p))
        Silent -> return ()

runScopeFileApp :: (Members '[App, EmbedIO, TaggedLock] r) => Sem (ScopeEff ': r) a -> Sem r a
runScopeFileApp = interpret $ \case
  ScopeFile p -> do
    let appFile =
          AppPath
            { _pathPath = mkPrepath (toFilePath p),
              _pathIsInput = False
            }
    ignoreProgressLog (runPipelineProgress () (Just appFile) upToScopingEntry)
  ScopeStdin e -> ignoreProgressLog (runPipelineEntry e upToScopingEntry)
