module Commands.Dev.Tree.Compile where

import Commands.Dev.Tree.Compile.Options
import Commands.Base

runCommand :: forall r. (Members '[EmbedIO, App, TaggedLock] r) => CompileCommand -> Sem r ()
runCommand = undefined
