module Casm.Reg.Base where

import Base
import Casm.Run.Base qualified as Run
import Data.Aeson
import Juvix.Compiler.Casm.Data.Result
import Juvix.Compiler.Casm.Error
import Juvix.Compiler.Casm.Interpreter
import Juvix.Compiler.Reg.Data.InfoTable qualified as Reg
import Juvix.Compiler.Reg.Transformation qualified as Reg
import Juvix.Data.PPOutput
import Reg.Run.Base qualified as Reg

compileAssertion' :: Maybe (Path Abs File) -> Path Abs Dir -> Path Abs File -> Symbol -> Reg.InfoTable -> (String -> IO ()) -> Assertion
compileAssertion' inputFile _ outputFile _ tab step = do
  step "Translate to CASM"
  case run $ runError @JuvixError $ runReader Reg.defaultOptions $ regToCasm' tab of
    Left err -> assertFailure (prettyString (fromJuvixError @GenericError err))
    Right Result {..} -> do
      step "Interpret"
      hout <- openFile (toFilePath outputFile) WriteMode
      rv <- Run.doRun hout _resultLabelInfo _resultCode inputFile
      case rv of
        Left e -> do
          hClose hout
          assertFailure (show (pretty (fromJuvixError @GenericError (JuvixError @CasmError e))))
        Right v -> do
          hPrint hout v
          hClose hout

cairoAssertion' :: Maybe (Path Abs File) -> Path Abs Dir -> Path Abs File -> Symbol -> Reg.InfoTable -> (String -> IO ()) -> Assertion
cairoAssertion' inputFile dirPath outputFile _ tab step = do
  step "Translate to Cairo"
  case run $ runError @JuvixError $ runReader Reg.defaultOptions $ regToCairo' tab of
    Left err -> assertFailure (prettyString (fromJuvixError @GenericError err))
    Right res -> do
      step "Serialize to Cairo bytecode"
      encodeFile (toFilePath outputFile) res
      step "Execute in Cairo VM"
      actualOutput <- Run.casmRunVM' dirPath outputFile inputFile
      writeFileEnsureLn outputFile actualOutput

regToCasmAssertion :: Path Abs File -> Maybe (Path Abs File) -> Path Abs File -> (String -> IO ()) -> Assertion
regToCasmAssertion mainFile inputFile expectedFile step =
  Reg.regRunAssertionParam (compileAssertion' inputFile) mainFile expectedFile [] (const (return ())) step

regToCairoAssertion :: Path Abs File -> Maybe (Path Abs File) -> Path Abs File -> (String -> IO ()) -> Assertion
regToCairoAssertion mainFile inputFile expectedFile step =
  Reg.regRunAssertionParam (cairoAssertion' inputFile) mainFile expectedFile [] (const (return ())) step
