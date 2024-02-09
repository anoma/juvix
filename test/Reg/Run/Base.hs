module Reg.Run.Base where

import Base
import Juvix.Compiler.Reg.Data.InfoTable
import Juvix.Compiler.Reg.Error
import Juvix.Compiler.Reg.Interpreter
import Juvix.Compiler.Reg.Pretty
import Juvix.Compiler.Reg.Translation.FromSource
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

regRunAssertion' :: InfoTable -> Path Abs File -> (String -> IO ()) -> Assertion
regRunAssertion' = regRunAssertionParam' runAssertion

regRunAssertionParam' :: (Handle -> Symbol -> InfoTable -> IO ()) -> InfoTable -> Path Abs File -> (String -> IO ()) -> Assertion
regRunAssertionParam' interpretFun tab expectedFile step = do
  step "Validate"
  case tab ^. infoMainFunction of
    Just sym -> do
      withTempDir'
        ( \dirPath -> do
            let outputFile = dirPath <//> $(mkRelFile "out.out")
            hout <- openFile (toFilePath outputFile) WriteMode
            step "Interpret"
            interpretFun hout sym tab
            hClose hout
            actualOutput <- readFile (toFilePath outputFile)
            step "Compare expected and actual program output"
            expected <- readFile (toFilePath expectedFile)
            assertEqDiffText ("Check: RUN output = " <> toFilePath expectedFile) actualOutput expected
        )
    Nothing -> assertFailure "no 'main' function"

regRunAssertion :: Path Abs File -> Path Abs File -> (InfoTable -> Either RegError InfoTable) -> (InfoTable -> Assertion) -> (String -> IO ()) -> Assertion
regRunAssertion = regRunAssertionParam runAssertion

regRunAssertionParam :: (Handle -> Symbol -> InfoTable -> IO ()) -> Path Abs File -> Path Abs File -> (InfoTable -> Either RegError InfoTable) -> (InfoTable -> Assertion) -> (String -> IO ()) -> Assertion
regRunAssertionParam interpretFun mainFile expectedFile trans testTrans step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left err -> assertFailure (show (pretty err))
    Right tab0 -> do
      case trans tab0 of
        Left err -> assertFailure (show (pretty err))
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
  let f' = toFilePath f
  s <- readFile f'
  return $ runParser f' s

doRun ::
  Handle ->
  InfoTable ->
  FunctionInfo ->
  IO (Either RegError Val)
doRun hout tab funInfo =
  runFinal
    . embedToFinal @IO
    . runError
    $ runFunctionIO stdin hout tab [] funInfo
