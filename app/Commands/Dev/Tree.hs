module Commands.Dev.Tree where

import Commands.Base
import Commands.Dev.Tree.Options
import Commands.Dev.Tree.Read as Read

runCommand :: forall r. (Members '[Embed IO, App, TaggedLock] r) => TreeCommand -> Sem r ()
runCommand = \case
  Read opts -> Read.runCommand opts
