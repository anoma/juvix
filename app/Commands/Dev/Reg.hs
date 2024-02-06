module Commands.Dev.Reg where

import Commands.Base
import Commands.Dev.Reg.Options
import Commands.Dev.Reg.Read as Read

runCommand :: forall r. (Members '[Embed IO, App, TaggedLock] r) => RegCommand -> Sem r ()
runCommand = \case
  Read opts -> Read.runCommand opts
