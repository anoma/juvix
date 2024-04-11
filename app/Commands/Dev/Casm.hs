module Commands.Dev.Casm where

import Commands.Base
import Commands.Dev.Casm.Compile as Compile
import Commands.Dev.Casm.FromCairo as FromCairo
import Commands.Dev.Casm.Options
import Commands.Dev.Casm.Read as Read
import Commands.Dev.Casm.Run as Run

runCommand :: forall r. (Members '[EmbedIO, App, TaggedLock] r) => CasmCommand -> Sem r ()
runCommand = \case
  Compile opts -> Compile.runCommand opts
  Run opts -> Run.runCommand opts
  Read opts -> Read.runCommand opts
  FromCairo opts -> FromCairo.runCommand opts
