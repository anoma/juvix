module Reg.Run.Base where

import Base
import Juvix.Compiler.Reg.Data.InfoTable
import Juvix.Compiler.Reg.Error
import Juvix.Compiler.Reg.Interpreter
import Juvix.Compiler.Reg.Pretty
import Juvix.Compiler.Reg.Transformation
import Juvix.Compiler.Reg.Translation.FromSource
import Juvix.Data.PPOutput

runAssertion :: Path Abs Dir -> Path Abs File -> Symbol -> InfoTable -> (String -> IO ()) -> Assertion
runAssertion _ outputFile sym tab step = do
  hout <- openFile (toFilePath outputFile) WriteMode
  step "Interpret"
  r' <- doRun hout tab (lookupFunInfo tab sym)
  case r' of
    Left err -> do
      hClose hout
      assertFailure (show (pretty err))
    Right value' -> do
      case value' of
        ValVoid ->
          hClose hout
        _ -> do
          hPutStrLn hout (ppPrint tab value')
          hClose hout

regRunAssertion' :: InfoTable -> Path Abs File -> (String -> IO ()) -> Assertion
regRunAssertion' = regRunAssertionParam' runAssertion

regRunAssertionParam' :: (Path Abs Dir -> Path Abs File -> Symbol -> InfoTable -> (String -> IO ()) -> Assertion) -> InfoTable -> Path Abs File -> (String -> IO ()) -> Assertion
regRunAssertionParam' interpretFun tab expectedFile step = do
  case tab ^. infoMainFunction of
    Just sym -> do
      withTempDir'
        ( \dirPath -> do
            let outputFile = dirPath <//> $(mkRelFile "out.out")
            interpretFun dirPath outputFile sym tab step
            actualOutput <- readFile outputFile
            step "Compare expected and actual program output"
            expected <- readFile expectedFile
            assertEqDiffText ("Check: RUN output = " <> toFilePath expectedFile) actualOutput expected
        )
    Nothing -> assertFailure "no 'main' function"

regRunAssertion :: Path Abs File -> Path Abs File -> [TransformationId] -> (InfoTable -> Assertion) -> (String -> IO ()) -> Assertion
regRunAssertion = regRunAssertionParam runAssertion

regRunAssertionParam :: (Path Abs Dir -> Path Abs File -> Symbol -> InfoTable -> (String -> IO ()) -> Assertion) -> Path Abs File -> Path Abs File -> [TransformationId] -> (InfoTable -> Assertion) -> (String -> IO ()) -> Assertion
regRunAssertionParam interpretFun mainFile expectedFile trans testTrans step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left err -> assertFailure (show (pretty err))
    Right tab0 -> do
      unless (null trans) $
        step "Transform"
      case run $ runError @JuvixError $ applyTransformations trans tab0 of
        Left err -> assertFailure (show (pretty (fromJuvixError @GenericError err)))
        Right tab -> do
          testTrans tab
          regRunAssertionParam' interpretFun tab expectedFile step

regRunErrorAssertion :: Path Abs File -> (String -> IO ()) -> Assertion
regRunErrorAssertion mainFile step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left _ -> assertBool "" True
    Right tab ->
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
  IO (Either RegError Val)
doRun hout tab funInfo =
  runM
    . runError
    $ runFunctionIO stdin hout tab [] funInfo
