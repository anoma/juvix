module Casm.Reg.Base where

import Base
import Casm.Run.Base qualified as Run
import Data.Aeson
import Juvix.Compiler.Casm.Data.Result
import Juvix.Compiler.Casm.Interpreter
import Juvix.Compiler.Reg.Data.InfoTable qualified as Reg
import Juvix.Data.PPOutput
import Reg.Run.Base qualified as Reg

compileAssertion' :: Path Abs Dir -> Path Abs File -> Symbol -> Reg.InfoTable -> (String -> IO ()) -> Assertion
compileAssertion' _ outputFile _ tab step = do
  step "Translate to CASM"
  case run $ runError @JuvixError $ regToCasm tab of
    Left err -> assertFailure (show (pretty (fromJuvixError @GenericError err)))
    Right Result {..} -> do
      hout <- openFile (toFilePath outputFile) WriteMode
      let v = hRunCode hout _resultLabelInfo _resultCode
      hPrint hout v
      hClose hout

cairoAssertion' :: Path Abs Dir -> Path Abs File -> Symbol -> Reg.InfoTable -> (String -> IO ()) -> Assertion
cairoAssertion' dirPath outputFile _ tab step = do
  step "Translate to Cairo"
  case run $ runError @JuvixError $ regToCairo tab of
    Left err -> assertFailure (show (pretty (fromJuvixError @GenericError err)))
    Right res -> do
      step "Serialize to Cairo bytecode"
      encodeFile (toFilePath outputFile) res
      step "Execute in Cairo VM"
      actualOutput <- Run.casmRunVM' dirPath outputFile
      writeFileEnsureLn outputFile actualOutput

regToCasmAssertion :: Path Abs File -> Path Abs File -> (String -> IO ()) -> Assertion
regToCasmAssertion mainFile expectedFile step =
  Reg.regRunAssertionParam compileAssertion' mainFile expectedFile [] (const (return ())) step

regToCairoAssertion :: Path Abs File -> Path Abs File -> (String -> IO ()) -> Assertion
regToCairoAssertion mainFile expectedFile step =
  Reg.regRunAssertionParam cairoAssertion' mainFile expectedFile [] (const (return ())) step
