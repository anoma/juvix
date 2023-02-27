module Commands.Dev.Geb
  ( module Commands.Dev.Geb,
    module Commands.Dev.Geb.Options,
  )
where

import Commands.Base
import Commands.Dev.Geb.Check as Infer
import Commands.Dev.Geb.Eval as Eval
import Commands.Dev.Geb.Infer as Check
import Commands.Dev.Geb.Options
import Commands.Dev.Geb.Read as Read
import Commands.Dev.Geb.Repl as Repl

runCommand :: forall r. (Members '[Embed IO, App] r) => GebCommand -> Sem r ()
runCommand = \case
  GebCommandRepl opts -> Repl.runCommand opts
  GebCommandEval opts -> Eval.runCommand opts
  GebCommandRead opts -> Read.runCommand opts
  GebCommandInfer opts -> Infer.runCommand opts
  GebCommandCheck opts -> Check.runCommand opts
