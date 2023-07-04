module VMInterpreter where

import App
import CommonOptions
import Juvix.Compiler.VM.Extra.Labels qualified as VM
import Juvix.Compiler.VM.Interpreter qualified as VM
import Juvix.Compiler.VM.Language qualified as VM

runVM :: forall r. (Members '[Embed IO, App] r) => [VM.Instruction] -> Sem r ()
runVM instrs = do
  r :: Either VM.LabelError [VM.Instruction] <- runError $ VM.resolveLabels instrs
  case r of
    Left err ->
      exitJuvixError (JuvixError err)
    Right instrs' ->
      embed (print (VM.runCode instrs'))
