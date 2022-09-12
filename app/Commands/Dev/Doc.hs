module Commands.Dev.Doc where

import Commands.Base
import Commands.Dev.Doc.Options
import Juvix.Compiler.Backend.Html.Translation.FromTyped qualified as Doc
import Juvix.Extra.Process
import System.Process qualified as Process

runCommand :: Members '[Embed IO, App] r => EntryPoint -> DocOptions -> Sem r ()
runCommand entryPoint localOpts = do
  ctx <-
    runPipeline (upToInternalTyped entryPoint)
  let docDir = localOpts ^. docOutputDir
  Doc.compile docDir "proj" ctx
  when (localOpts ^. docOpen) $ case openCmd of
    Nothing -> say "Could not recognize the 'open' command for your OS"
    Just opencmd -> embed (void (Process.spawnProcess opencmd [docDir </> Doc.indexFileName]))
