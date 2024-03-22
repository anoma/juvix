module Commands.Dev.Nockma where

import Commands.Base
import Commands.Dev.Nockma.Eval as Eval
import Commands.Dev.Nockma.Format as Format
import Commands.Dev.Nockma.Options
import Commands.Dev.Nockma.Repl as Repl
import Commands.Dev.Nockma.Run as Run

runCommand :: forall r. (Members '[EmbedIO, App] r) => NockmaCommand -> Sem r ()
runCommand = \case
  NockmaRepl opts -> Repl.runCommand opts
  NockmaEval opts -> Eval.runCommand opts
  NockmaFormat opts -> Format.runCommand opts
  NockmaRun opts -> Run.runCommand opts
