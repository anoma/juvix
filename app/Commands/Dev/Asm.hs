module Commands.Dev.Asm where

import Commands.Base
import Commands.Dev.Asm.Options
import Commands.Dev.Asm.Run as Run

runCommand :: forall r. Members '[Embed IO, App] r => AsmCommand -> Sem r ()
runCommand = \case
  Run opts -> Run.runCommand opts
