module Commands.Dev.Casm where

import Commands.Base
import Commands.Dev.Casm.Options
import Commands.Dev.Casm.Read as Read
import Commands.Dev.Casm.Run as Run

runCommand :: forall r. (Members '[EmbedIO, App, TaggedLock] r) => CasmCommand -> Sem r ()
runCommand = \case
  Run opts -> Run.runCommand opts
  Read opts -> Read.runCommand opts
