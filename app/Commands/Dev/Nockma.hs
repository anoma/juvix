module Commands.Dev.Nockma where

import Commands.Base
import Commands.Dev.Nockma.FromAsm as FromAsm
import Commands.Dev.Nockma.Options
import Commands.Dev.Nockma.Repl as Repl

runCommand :: forall r. (Members '[Embed IO, App] r) => NockmaCommand -> Sem r ()
runCommand = \case
  NockmaRepl opts -> Repl.runCommand opts
  NockmaFromAsm opts -> FromAsm.runCommand opts
