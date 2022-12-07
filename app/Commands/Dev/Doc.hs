module Commands.Dev.Doc where

import Commands.Base
import Commands.Dev.Doc.Options
import Juvix.Compiler.Backend.Html.Translation.FromTyped qualified as Doc
import Juvix.Extra.Process
import System.Process qualified as Process

runCommand :: Members '[Embed IO, App] r => DocOptions -> Sem r ()
runCommand DocOptions {..} = do
  ctx <- runPipeline _docInputFile upToInternalTyped
  docDir <- someBaseToAbs' (_docOutputDir ^. pathPath)
  Doc.compile docDir "proj" ctx
  when _docOpen $ case openCmd of
    Nothing -> say "Could not recognize the 'open' command for your OS"
    Just opencmd -> embed (void (Process.spawnProcess opencmd [toFilePath (docDir <//> Doc.indexFileName)]))
