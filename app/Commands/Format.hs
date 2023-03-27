module Commands.Format where

import Commands.Base
import Commands.Format.Options
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

runCommand :: forall r. Members '[Embed IO, App, Resource, Files] r => FormatOptions -> Sem r ()
runCommand opts = runOutputSem (renderFormattedOutput opts) $ runScopeFileApp $ do
  res <- case opts ^. formatInput of
    Left appFile -> do
      p <- someBaseToAbs' (appFile ^. pathPath)
      format p
    Right rootPath -> do
      p <- someBaseToAbs' (rootPath ^. pathPath)
      formatProject p
  when (res == FormatResultFail) (exitMsg (ExitFailure 1) "")

renderModeFromOptions :: FormatOptions -> FormattedFileInfo -> FormatRenderMode
renderModeFromOptions opts formattedInfo
  | opts ^. formatInPlace = EditInPlace formattedInfo
  | opts ^. formatCheck = NoEdit Silent
  | otherwise = case target of
      TargetFile -> NoEdit (ReformattedFile (formattedInfo ^. formattedFileInfoContentsAnsi))
      TargetDir -> NoEdit (InputPath (formattedInfo ^. formattedFileInfoPath))
  where
    target :: FormatTarget
    target = targetFromOptions opts

targetFromOptions :: FormatOptions -> FormatTarget
targetFromOptions opts = case (opts ^. formatInput) of
  Left {} -> TargetFile
  Right {} -> TargetDir

renderFormattedOutput :: forall r. Members '[Embed IO, App, Resource, Files] r => FormatOptions -> FormattedFileInfo -> Sem r ()
renderFormattedOutput opts fInfo = do
  let renderMode = renderModeFromOptions opts fInfo
  outputResult renderMode
  where
    outputResult :: FormatRenderMode -> Sem r ()
    outputResult = \case
      EditInPlace i@(FormattedFileInfo {..}) ->
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
    let appFile = AppPath (Abs p) False
    runPipeline appFile upToScoping
