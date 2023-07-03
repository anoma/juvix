module Commands.Dev.VM.Run where

import Commands.Base
import Commands.Dev.VM.Run.Options
import Juvix.Compiler.VM.Translation.FromSource qualified as VM
import VMInterpreter

runCommand :: forall r. (Members '[Embed IO, App] r) => VMRunOptions -> Sem r ()
runCommand opts = do
  afile :: Path Abs File <- fromAppPathFile file
  s <- embed (readFile (toFilePath afile))
  case VM.runParser (toFilePath afile) s of
    Left err -> exitJuvixError (JuvixError err)
    Right instrs -> runVM instrs
  where
    file :: AppPath File
    file = opts ^. vmRunInputFile
