module Commands.Dev.VM where

import Commands.Base
import Commands.Dev.VM.Options
import Commands.Dev.VM.Run as Run

runCommand :: forall r. (Members '[Embed IO, App] r) => VMCommand -> Sem r ()
runCommand = \case
  Run opts -> Run.runCommand opts
