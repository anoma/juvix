module Casm.Run.Base where

import Base
import Juvix.Compiler.Casm.Error
import Juvix.Compiler.Casm.Interpreter
import Juvix.Compiler.Casm.Translation.FromSource
import Juvix.Compiler.Casm.Validate
import Juvix.Data.PPOutput
import Juvix.Parser.Error

casmRunAssertion' :: LabelInfo -> Code -> Path Abs File -> (String -> IO ()) -> Assertion
casmRunAssertion' labi instrs expectedFile step =
  case validate labi instrs of
    Left err -> do
      assertFailure (show (pretty err))
    Right () -> do
      withTempDir'
        ( \dirPath -> do
            let outputFile = dirPath <//> $(mkRelFile "out.out")
            step "Interpret"
            r' <- doRun labi instrs
            case r' of
              Left err -> do
                assertFailure (show (pretty err))
              Right value' -> do
                hout <- openFile (toFilePath outputFile) WriteMode
                hPrint hout value'
                hClose hout
                actualOutput <- readFile outputFile
                step "Compare expected and actual program output"
                expected <- readFile expectedFile
                assertEqDiffText ("Check: RUN output = " <> toFilePath expectedFile) actualOutput expected
        )

casmRunAssertion :: Path Abs File -> Path Abs File -> (String -> IO ()) -> Assertion
casmRunAssertion mainFile expectedFile step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left err -> assertFailure (show (pretty err))
    Right (labi, instrs) -> casmRunAssertion' labi instrs expectedFile step

casmRunErrorAssertion :: Path Abs File -> (String -> IO ()) -> Assertion
casmRunErrorAssertion mainFile step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left _ -> assertBool "" True
    Right (labi, instrs) -> do
      step "Validate"
      case validate labi instrs of
        Left {} -> assertBool "" True
        Right () -> do
          step "Interpret"
          r' <- doRun labi instrs
          case r' of
            Left _ -> assertBool "" True
            Right _ -> assertFailure "no error"

parseFile :: Path Abs File -> IO (Either MegaparsecError (LabelInfo, Code))
parseFile f = do
  s <- readFile f
  return (runParser f s)

doRun ::
  LabelInfo ->
  Code ->
  IO (Either CasmError Integer)
doRun labi instrs = catchRunErrorIO (runCode labi instrs)
