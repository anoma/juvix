module Commands.Dev.Reg where

import Commands.Base
import Commands.Dev.Reg.Compile as Compile
import Commands.Dev.Reg.Options
import Commands.Dev.Reg.Read as Read
import Commands.Dev.Reg.Run as Run

runCommand :: forall r. (Members '[EmbedIO, App, TaggedLock] r) => RegCommand -> Sem r ()
runCommand = \case
  Compile opts -> Compile.runCommand opts
  Run opts -> Run.runCommand opts
  Read opts -> Read.runCommand opts
