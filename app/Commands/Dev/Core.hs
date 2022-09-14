module Commands.Dev.Core where

import Commands.Base
import Commands.Dev.Core.Eval as Eval
import Commands.Dev.Core.Options
import Commands.Dev.Core.Read as Read
import Commands.Dev.Core.Repl as Repl

runCommand :: forall r. Members '[Embed IO, App] r => CoreCommand -> Sem r ()
runCommand = \case
  Repl opts -> Repl.runCommand opts
  Eval opts -> Eval.runCommand opts
  Read opts -> Read.runCommand opts
