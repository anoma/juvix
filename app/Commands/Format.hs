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
runCommand opts = do
  f <- filePathToAbs (opts ^. formatInput)
  let target = case f of
        Left {} -> TargetFile
        Right {} -> TargetDir
  runOutputSem (renderFormattedOutput target opts) $ runScopeFileApp $ do
    res <- case f of
      Left p -> format p
      Right p -> formatProject p
    when (res == FormatResultFail) (embed (exitWith (ExitFailure 1)))

renderModeFromOptions :: FormatTarget -> FormatOptions -> FormattedFileInfo -> FormatRenderMode
renderModeFromOptions target opts formattedInfo
  | opts ^. formatInPlace = EditInPlace formattedInfo
  | opts ^. formatCheck = NoEdit Silent
  | otherwise = case target of
      TargetFile -> NoEdit (ReformattedFile (formattedInfo ^. formattedFileInfoContentsAnsi))
      TargetDir -> NoEdit (InputPath (formattedInfo ^. formattedFileInfoPath))

renderFormattedOutput :: forall r. Members '[Embed IO, App, Resource, Files] r => FormatTarget -> FormatOptions -> FormattedFileInfo -> Sem r ()
renderFormattedOutput target opts fInfo = do
  let renderMode = renderModeFromOptions target opts fInfo
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
    let appFile =
          AppPath
            { _pathPath = mkPrepath (toFilePath p),
              _pathIsInput = False
            }
    runPipeline appFile upToScoping
