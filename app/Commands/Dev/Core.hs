module Commands.Dev.Core where

import Commands.Base
import Commands.Dev.Core.Asm as Asm
import Commands.Dev.Core.Compile as Compile
import Commands.Dev.Core.Eval as Eval
import Commands.Dev.Core.FromConcrete as FromConcrete
import Commands.Dev.Core.Options
import Commands.Dev.Core.Read as Read
import Commands.Dev.Core.Repl as Repl
import Commands.Dev.Core.Strip as Strip

runCommand :: forall r. (Members '[Embed IO, App] r) => CoreCommand -> Sem r ()
runCommand = \case
  Repl opts -> Repl.runCommand opts
  Eval opts -> Eval.runCommand opts
  Read opts -> Read.runCommand opts
  FromConcrete opts -> FromConcrete.runCommand opts
  Strip opts -> Strip.runCommand opts
  CoreAsm opts -> Asm.runCommand opts
  CoreCompile opts -> Compile.runCommand opts
