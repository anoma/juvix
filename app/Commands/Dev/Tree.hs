module Commands.Dev.Tree where

import Commands.Base
import Commands.Dev.Tree.Eval as Eval
import Commands.Dev.Tree.FromAsm as FromAsm
import Commands.Dev.Tree.Options
import Commands.Dev.Tree.Read as Read

runCommand :: forall r. (Members '[Embed IO, App, TaggedLock] r) => TreeCommand -> Sem r ()
runCommand = \case
  Eval opts -> Eval.runCommand opts
  Read opts -> Read.runCommand opts
  FromAsm opts -> FromAsm.runCommand opts
