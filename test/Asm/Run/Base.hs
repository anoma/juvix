module Asm.Run.Base where

import Base
import Data.Text.IO qualified as TIO
import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Error
import Juvix.Compiler.Asm.Extra
import Juvix.Compiler.Asm.Interpreter
import Juvix.Compiler.Asm.Pretty
import Juvix.Compiler.Asm.Transformation.Validate
import Juvix.Compiler.Asm.Translation.FromSource
import Juvix.Data.PPOutput
import System.IO.Extra (withTempDir)

asmRunAssertion :: FilePath -> FilePath -> (String -> IO ()) -> Assertion
asmRunAssertion mainFile expectedFile step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left err -> assertFailure (show (pretty err))
    Right tab -> do
      step "Validate"
      case validate' tab of
        Just err -> assertFailure (show (pretty err))
        Nothing ->
          case tab ^. infoMainFunction of
            Just sym -> do
              withTempDir
                ( \dirPath -> do
                    let outputFile = dirPath </> "out.out"
                    hout <- openFile outputFile WriteMode
                    step "Interpret"
                    r' <- doRun hout tab (getFunInfo tab sym)
                    case r' of
                      Left err -> do
                        hClose hout
                        assertFailure (show (pretty err))
                      Right value' -> do
                        case value' of
                          ValUnit (Unit False) -> return ()
                          _ -> hPutStrLn hout (ppPrint tab value')
                        hClose hout
                        actualOutput <- TIO.readFile outputFile
                        step "Compare expected and actual program output"
                        expected <- TIO.readFile expectedFile
                        assertEqDiff ("Check: RUN output = " <> expectedFile) actualOutput expected
                )
            Nothing -> assertFailure "no 'main' function"

asmRunErrorAssertion :: FilePath -> (String -> IO ()) -> Assertion
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
              withTempDir
                ( \dirPath -> do
                    let outputFile = dirPath </> "out.out"
                    hout <- openFile outputFile WriteMode
                    step "Interpret"
                    r' <- doRun hout tab (getFunInfo tab sym)
                    hClose hout
                    case r' of
                      Left _ -> assertBool "" True
                      Right _ -> assertFailure "no error"
                )
            Nothing -> assertBool "" True

parseFile :: FilePath -> IO (Either ParserError InfoTable)
parseFile f = do
  s <- readFile f
  return $ runParser f s

doRun ::
  Handle ->
  InfoTable ->
  FunctionInfo ->
  IO (Either AsmError Val)
doRun hout tab funInfo = catchRunErrorIO (hRunCodeIO stdin hout tab funInfo)
