module Casm.Run.Base where

import Base
import Data.Aeson
import Juvix.Compiler.Casm.Data.Result qualified as Casm
import Juvix.Compiler.Casm.Error
import Juvix.Compiler.Casm.Interpreter
import Juvix.Compiler.Casm.Translation.FromSource
import Juvix.Compiler.Casm.Validate
import Juvix.Data.Field
import Juvix.Data.PPOutput
import Juvix.Parser.Error
import Runtime.Base qualified as R

casmRunVM' :: Path Abs Dir -> Path Abs File -> IO Text
casmRunVM' dirPath outputFile = do
  dir <- getCurrentDir
  setCurrentDir dirPath
  out0 <- R.readProcess "run_cairo_vm.sh" [toFilePath outputFile] ""
  setCurrentDir dir
  return $ fromString $ unlines $ drop 1 $ lines (fromText out0)

casmRunVM :: LabelInfo -> Code -> Path Abs File -> (String -> IO ()) -> Assertion
casmRunVM labi instrs expectedFile step = do
  step "Check run_cairo_vm.sh is on path"
  assertCmdExists $(mkRelFile "run_cairo_vm.sh")
  withTempDir'
    ( \dirPath -> do
        step "Serialize to Cairo bytecode"
        let res = run $ casmToCairo (Casm.Result labi instrs)
            outputFile = dirPath <//> $(mkRelFile "out.json")
        encodeFile (toFilePath outputFile) res
        step "Run Cairo VM"
        actualOutput <- casmRunVM' dirPath outputFile
        step "Compare expected and actual program output"
        expected <- readFile expectedFile
        assertEqDiffText ("Check: RUN output = " <> toFilePath expectedFile) actualOutput expected
    )

casmRunAssertion' :: Bool -> LabelInfo -> Code -> Path Abs File -> (String -> IO ()) -> Assertion
casmRunAssertion' bRunVM labi instrs expectedFile step =
  case validate labi instrs of
    Left err -> do
      assertFailure (show (pretty err))
    Right () -> do
      withTempDir'
        ( \dirPath -> do
            let outputFile = dirPath <//> $(mkRelFile "out.out")
            step "Interpret"
            hout <- openFile (toFilePath outputFile) WriteMode
            r' <- doRun hout labi instrs
            case r' of
              Left err -> do
                hClose hout
                assertFailure (show (pretty err))
              Right value' -> do
                hPrint hout value'
                hClose hout
                actualOutput <- readFile outputFile
                step "Compare expected and actual program output"
                expected <- readFile expectedFile
                assertEqDiffText ("Check: RUN output = " <> toFilePath expectedFile) actualOutput expected
        )
      when bRunVM $
        casmRunVM labi instrs expectedFile step

casmRunAssertion :: Bool -> Path Abs File -> Path Abs File -> (String -> IO ()) -> Assertion
casmRunAssertion bRunVM mainFile expectedFile step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left err -> assertFailure (show (pretty err))
    Right (labi, instrs) -> casmRunAssertion' bRunVM labi instrs expectedFile step

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
          r' <- doRun stderr labi instrs
          case r' of
            Left _ -> assertBool "" True
            Right _ -> assertFailure "no error"

parseFile :: Path Abs File -> IO (Either MegaparsecError (LabelInfo, Code))
parseFile f = do
  s <- readFile f
  return (runParser f s)

doRun ::
  Handle ->
  LabelInfo ->
  Code ->
  IO (Either CasmError FField)
doRun hout labi instrs = catchRunErrorIO (hRunCode hout labi instrs)
