module Commands.Format where

import Commands.Base
import Commands.Format.Options
import Data.Text qualified as Text
import Juvix.Formatter
import Juvix.Prelude.Pretty

data FormatNoEditRenderMode
  = ReformattedFile (NonEmpty AnsiText)
  | InputPath (Path Abs File)
  | Silent

data FormatRenderMode
  = EditInPlace FormattedFileInfo
  | NoEdit FormatNoEditRenderMode

data FormatTarget
  = TargetFile
  | TargetDir
  | TargetStdin

runCommand :: forall r. Members '[Embed IO, App, Resource, Files] r => FormatOptions -> Sem r ()
runCommand opts = do
  globalOpts <- askGlobalOptions
  let isStdin = globalOpts ^. globalStdin
  f <- mapM filePathToAbs (opts ^. formatInput)

  let target = case f of
        Just Left {} -> TargetFile
        Just Right {} -> TargetDir
        Nothing -> TargetStdin
  runOutputSem (renderFormattedOutput target opts) $ runScopeFileApp $ do
    res <- case f of
      Just (Left p) -> format p
      Just (Right p) -> formatProject p
      Nothing ->
        if
            | isStdin -> formatStdin
            | otherwise -> do
                printFailureExit $
                  Text.unlines
                    [ "juvix format error: either 'JUVIX_FILE_OR_PROJECT' or '--stdin' option must be specified",
                      "Use the --help option to display more usage information."
                    ]
                return FormatResultFail
    when (res == FormatResultFail) (embed (exitWith (ExitFailure 1)))

renderModeFromOptions :: FormatTarget -> FormatOptions -> FormattedFileInfo -> FormatRenderMode
renderModeFromOptions target opts formattedInfo
  | opts ^. formatInPlace = whenContentsModified (EditInPlace formattedInfo)
  | opts ^. formatCheck = NoEdit Silent
  | otherwise = case target of
      TargetFile -> NoEdit (ReformattedFile (formattedInfo ^. formattedFileInfoContentsAnsi))
      TargetDir -> whenContentsModified (NoEdit (InputPath (formattedInfo ^. formattedFileInfoPath)))
      TargetStdin -> NoEdit (ReformattedFile (formattedInfo ^. formattedFileInfoContentsAnsi))
  where
    whenContentsModified :: FormatRenderMode -> FormatRenderMode
    whenContentsModified res
      | formattedInfo ^. formattedFileInfoContentsModified = res
      | otherwise = NoEdit Silent

renderFormattedOutput :: forall r. Members '[Embed IO, App, Resource, Files] r => FormatTarget -> FormatOptions -> FormattedFileInfo -> Sem r ()
renderFormattedOutput target opts fInfo = do
  let renderMode = renderModeFromOptions target opts fInfo
  outputResult renderMode
  where
    outputResult :: FormatRenderMode -> Sem r ()
    outputResult = \case
      EditInPlace i@FormattedFileInfo {..} ->
        runTempFileIO $
          restoreFileOnError _formattedFileInfoPath $
            writeFile' _formattedFileInfoPath (i ^. formattedFileInfoContentsText)
      NoEdit m -> case m of
        ReformattedFile ts -> forM_ ts renderStdOut
        InputPath p -> say (pack (toFilePath p))
        Silent -> return ()

runScopeFileApp :: Member App r => Sem (ScopeEff ': r) a -> Sem r a
runScopeFileApp = interpret $ \case
  ScopeFile p -> do
    let appFile =
          AppPath
            { _pathPath = mkPrepath (toFilePath p),
              _pathIsInput = False
            }
    runPipeline appFile upToScoping
  ScopeStdin -> runPipelineNoFile upToScoping
