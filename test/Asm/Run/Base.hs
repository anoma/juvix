module Asm.Run.Base where

import Base
import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Error
import Juvix.Compiler.Asm.Interpreter
import Juvix.Compiler.Asm.Pretty
import Juvix.Compiler.Asm.Transformation.Validate
import Juvix.Compiler.Asm.Translation.FromSource
import Juvix.Data.PPOutput

runAssertion :: Handle -> Symbol -> InfoTable -> IO ()
runAssertion hout sym tab = do
  r' <- doRun hout tab (lookupFunInfo tab sym)
  case r' of
    Left err -> do
      hClose hout
      assertFailure (show (pretty err))
    Right value' -> do
      case value' of
        ValVoid -> return ()
        _ -> hPutStrLn hout (ppPrint tab value')

asmRunAssertion' :: InfoTable -> Path Abs File -> (String -> IO ()) -> Assertion
asmRunAssertion' = asmRunAssertionParam' runAssertion

asmRunAssertionParam' :: (Handle -> Symbol -> InfoTable -> IO ()) -> InfoTable -> Path Abs File -> (String -> IO ()) -> Assertion
asmRunAssertionParam' interpretFun tab expectedFile step = do
  step "Validate"
  case validate' tab of
    Just err -> assertFailure (show (pretty err))
    Nothing ->
      case tab ^. infoMainFunction of
        Just sym -> do
          withTempDir'
            ( \dirPath -> do
                let outputFile = dirPath <//> $(mkRelFile "out.out")
                hout <- openFile (toFilePath outputFile) WriteMode
                step "Interpret"
                interpretFun hout sym tab
                hClose hout
                actualOutput <- readFile outputFile
                step "Compare expected and actual program output"
                expected <- readFile expectedFile
                assertEqDiffText ("Check: RUN output = " <> toFilePath expectedFile) actualOutput expected
            )
        Nothing -> assertFailure "no 'main' function"

asmRunAssertion :: Path Abs File -> Path Abs File -> (InfoTable -> Either AsmError InfoTable) -> (InfoTable -> Assertion) -> (String -> IO ()) -> Assertion
asmRunAssertion = asmRunAssertionParam runAssertion

asmRunAssertionParam :: (Handle -> Symbol -> InfoTable -> IO ()) -> Path Abs File -> Path Abs File -> (InfoTable -> Either AsmError InfoTable) -> (InfoTable -> Assertion) -> (String -> IO ()) -> Assertion
asmRunAssertionParam interpretFun mainFile expectedFile trans testTrans step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left err -> assertFailure (show (pretty err))
    Right tab0 -> do
      case trans tab0 of
        Left err -> assertFailure (show (pretty err))
        Right tab -> do
          testTrans tab
          asmRunAssertionParam' interpretFun tab expectedFile step

asmRunErrorAssertion :: Path Abs File -> (String -> IO ()) -> Assertion
asmRunErrorAssertion mainFile step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left _ -> assertBool "" True
    Right tab -> do
      step "Validate"
      case validate' tab of
        Just _ -> assertBool "" True
        Nothing ->
          case tab ^. infoMainFunction of
            Just sym -> do
              withTempDir'
                ( \dirPath -> do
                    let outputFile = dirPath <//> $(mkRelFile "out.out")
                    hout <- openFile (toFilePath outputFile) WriteMode
                    step "Interpret"
                    r' <- doRun hout tab (lookupFunInfo tab sym)
                    hClose hout
                    case r' of
                      Left _ -> assertBool "" True
                      Right _ -> assertFailure "no error"
                )
            Nothing -> assertBool "" True

parseFile :: Path Abs File -> IO (Either MegaparsecError InfoTable)
parseFile f = do
  s <- readFile f
  return (runParser f s)

doRun ::
  Handle ->
  InfoTable ->
  FunctionInfo ->
  IO (Either AsmError Val)
doRun hout tab funInfo = catchRunErrorIO (hRunCodeIO stdin hout tab funInfo)
