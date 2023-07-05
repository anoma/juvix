module VMInterpreter where

import App
import CommonOptions
import Data.ByteString.Lazy qualified as BS
import Juvix.Compiler.VM.Extra.Labels qualified as VM
import Juvix.Compiler.VM.Interpreter qualified as VM
import Juvix.Compiler.VM.Language qualified as VM
import Juvix.Compiler.VM.Options qualified as VM
import Juvix.Prelude.Aeson

runVM ::
  forall r.
  (Members '[Embed IO, App] r) =>
  VM.Options ->
  [VM.Instruction] ->
  Sem r ()
runVM opts instrs = do
  mp <- case opts ^. VM.optInputsFile of
    Just file -> do
      r <- eitherDecode <$> embed (BS.readFile (toFilePath file))
      case r of
        Left err -> exitMsg (ExitFailure 1) (fromString err)
        Right m -> return $ m ^. jsonIntDataMap
    Nothing -> return mempty
  r :: Either VM.LabelError [VM.Instruction] <- runError $ VM.resolveLabels instrs
  case r of
    Left err ->
      exitJuvixError (JuvixError err)
    Right instrs' ->
      embed (print (VM.runCode opts mp instrs'))
