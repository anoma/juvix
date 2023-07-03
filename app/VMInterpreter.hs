module VMInterpreter where

import App
import CommonOptions
import Juvix.Compiler.VM.Interpreter qualified as VM
import Juvix.Compiler.VM.Language qualified as VM

runVM :: forall r. (Members '[Embed IO, App] r) => [VM.Instruction] -> Sem r ()
runVM instrs =
  embed (print (VM.runCode instrs))
