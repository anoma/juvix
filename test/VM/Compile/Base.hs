module VM.Compile.Base where

import Base
import Data.ByteString qualified as BS
import Juvix.Compiler.VM.Extra.Labels
import Juvix.Compiler.VM.Language
import Juvix.Compiler.VM.Options qualified as VM
import Juvix.Compiler.VM.Serialization.ToVampIR qualified as VM
import Juvix.Compiler.VM.Translation.FromSource
import Juvix.Data.PPOutput
import VampIR.Core.Base (VampirBackend (VampirHalo2), vampirRunAssertion)

vmCompileAssertion' :: VM.Options -> [Instruction] -> Path Abs File -> (String -> IO ()) -> Assertion
vmCompileAssertion' opts instrs dataFile step = do
  let r :: Either LabelError [Instruction] = run $ runError $ resolveLabels instrs
  case r of
    Left err ->
      assertFailure (show (pretty err))
    Right instrs' ->
      withTempDir'
        ( \dirPath -> do
            step "Translate to VampIR"
            let vampirFile = dirPath <//> $(mkRelFile "program.pir")
            case run (runError @LabelError (VM.serialize opts instrs')) of
              Left err -> assertFailure (show (pretty err))
              Right res -> do
                BS.writeFile (toFilePath vampirFile) res
                vampirRunAssertion VampirHalo2 dirPath dataFile vampirFile step
        )

vmCompileAssertion :: VM.Options -> Path Abs File -> Path Abs File -> (String -> IO ()) -> Assertion
vmCompileAssertion opts mainFile dataFile step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left err -> assertFailure (show (pretty err))
    Right instrs -> do
      vmCompileAssertion' opts instrs dataFile step

parseFile :: Path Abs File -> IO (Either MegaparsecError [Instruction])
parseFile f = do
  let f' = toFilePath f
  s <- readFile f'
  return $ runParser f' s
