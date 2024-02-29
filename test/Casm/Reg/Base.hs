module Casm.Reg.Base where

import Base
import Juvix.Compiler.Casm.Data.Result
import Juvix.Compiler.Casm.Interpreter
import Juvix.Compiler.Reg.Data.InfoTable qualified as Reg
import Juvix.Data.PPOutput
import Reg.Run.Base qualified as Reg

compileAssertion' :: Handle -> Symbol -> Reg.InfoTable -> IO ()
compileAssertion' hout _ tab = do
  case run $ runError @JuvixError $ regToCasm tab of
    Left err -> assertFailure (show (pretty (fromJuvixError @GenericError err)))
    Right Result {..} -> do
      let v = hRunCode hout _resultLabelInfo _resultCode
      hPutStrLn hout (show v)

regToCasmAssertion :: Path Abs File -> Path Abs File -> (String -> IO ()) -> Assertion
regToCasmAssertion mainFile expectedFile step =
  Reg.regRunAssertionParam compileAssertion' mainFile expectedFile [] (const (return ())) step
