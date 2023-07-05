module VMInterpreter where

import App
import CommonOptions
import Juvix.Compiler.VM.Extra.Labels qualified as VM
import Juvix.Compiler.VM.Interpreter qualified as VM
import Juvix.Compiler.VM.Language qualified as VM
import Juvix.Compiler.VM.Options qualified as VM

runVM :: forall r a. (Members '[Embed IO, App] r, CanonicalProjection a VM.Options) => a -> [VM.Instruction] -> Sem r ()
runVM opts instrs = do
  r :: Either VM.LabelError [VM.Instruction] <- runError $ VM.resolveLabels instrs
  case r of
    Left err ->
      exitJuvixError (JuvixError err)
    Right instrs' ->
      embed (print (VM.runCode (project opts) instrs'))
