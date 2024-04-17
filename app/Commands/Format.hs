module Commands.Format where

import Commands.Base
import Commands.Format.Options
import Data.Text qualified as Text
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
  | TargetProject (Path Abs Dir)
  | TargetStdin

isTargetProject :: FormatTarget -> Bool
isTargetProject = \case
  TargetProject {} -> True
  _ -> False

targetFromOptions :: (Members '[EmbedIO, App] r) => FormatOptions -> Sem r FormatTarget
targetFromOptions opts = do
  globalOpts <- askGlobalOptions
  let isStdin = globalOpts ^. globalStdin
  f <- mapM filePathToAbs (opts ^. formatInput)
  pkgDir <- askPkgDir
  case f of
    Just (Left p) -> return (TargetFile p)
    Just Right {} -> return (TargetProject pkgDir)
    Nothing -> do
      isPackageGlobal <- askPackageGlobal
      if
          | isStdin -> return TargetStdin
          | not (isPackageGlobal) -> return (TargetProject pkgDir)
          | otherwise -> do
              exitFailMsg $
                Text.unlines
                  [ "juvix format error: either 'JUVIX_FILE_OR_PROJECT' or '--stdin' option must be specified",
                    "Use the --help option to display more usage information."
                  ]

runCommand :: forall r. (Members '[EmbedIO, App, TaggedLock, Files] r) => FormatOptions -> Sem r ()
runCommand opts = do
  target <- targetFromOptions opts
  runOutputSem (renderFormattedOutput target opts) $ runScopeFileApp $ do
    res <- case target of
      TargetFile p -> format p
      TargetProject p -> formatProject p
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
    runPipelineNoOptions (Just appFile) upToScoping
  ScopeStdin e -> runPipelineEntry e upToScoping
