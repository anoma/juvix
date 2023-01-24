module Commands.Dev.Runtime where

import Commands.Base
import Commands.Dev.Runtime.Compile as Compile
import Commands.Dev.Runtime.Options

runCommand :: forall r. (Members '[Embed IO, App] r) => RuntimeCommand -> Sem r ()
runCommand = \case
  Compile opts -> Compile.runCommand opts
