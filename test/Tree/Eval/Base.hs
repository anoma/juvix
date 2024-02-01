module Tree.Eval.Base where

import Base
import Juvix.Compiler.Tree.Data.InfoTable
import Juvix.Compiler.Tree.Data.TransformationId
import Juvix.Compiler.Tree.Error
import Juvix.Compiler.Tree.Evaluator
import Juvix.Compiler.Tree.Language.Base
import Juvix.Compiler.Tree.Language.Value
import Juvix.Compiler.Tree.Pretty
import Juvix.Compiler.Tree.Transformation
import Juvix.Compiler.Tree.Translation.FromSource
import Juvix.Data.PPOutput

treeEvalAssertion ::
  Path Abs File ->
  Path Abs File ->
  [TransformationId] ->
  (InfoTable -> Assertion) ->
  (String -> IO ()) ->
  Assertion
treeEvalAssertion = treeEvalAssertionParam evalAssertion

treeEvalAssertionParam ::
  (Handle -> Symbol -> InfoTable -> IO ()) ->
  Path Abs File ->
  Path Abs File ->
  [TransformationId] ->
  (InfoTable -> Assertion) ->
  (String -> IO ()) ->
  Assertion
treeEvalAssertionParam evalParam mainFile expectedFile trans testTrans step = do
  step "Parse"
  s <- readFile (toFilePath mainFile)
  case runParser (toFilePath mainFile) s of
    Left err -> assertFailure (show (pretty err))
    Right tab0 -> do
      unless (null trans) $
        step "Transform"
      case run $ runError @JuvixError $ applyTransformations trans tab0 of
        Left err -> assertFailure (show (pretty (fromJuvixError @GenericError err)))
        Right tab -> do
          testTrans tab
          case tab ^. infoMainFunction of
            Just sym -> do
              withTempDir'
                ( \dirPath -> do
                    let outputFile = dirPath <//> $(mkRelFile "out.out")
                    hout <- openFile (toFilePath outputFile) WriteMode
                    step "Evaluate"
                    evalParam hout sym tab
                    hClose hout
                    actualOutput <- readFile (toFilePath outputFile)
                    step "Compare expected and actual program output"
                    expected <- readFile (toFilePath expectedFile)
                    assertEqDiffText ("Check: RUN output = " <> toFilePath expectedFile) actualOutput expected
                )
            Nothing -> assertFailure "no 'main' function"

evalAssertion :: Handle -> Symbol -> InfoTable -> IO ()
evalAssertion hout sym tab = do
  r' <- doEval hout tab (lookupFunInfo tab sym)
  case r' of
    Left err -> do
      hClose hout
      assertFailure (show (pretty err))
    Right value' -> do
      case value' of
        ValVoid -> return ()
        _ -> hPutStrLn hout (ppPrint tab value')

doEval ::
  Handle ->
  InfoTable ->
  FunctionInfo ->
  IO (Either TreeError Value)
doEval hout tab funInfo = catchEvalErrorIO (hEvalIO stdin hout tab funInfo)

treeEvalErrorAssertion :: Path Abs File -> (String -> IO ()) -> Assertion
treeEvalErrorAssertion mainFile step = do
  step "Parse"
  s <- readFile (toFilePath mainFile)
  case runParser (toFilePath mainFile) s of
    Left err -> assertFailure (show (pretty err))
    Right tab ->
      case tab ^. infoMainFunction of
        Just sym -> do
          withTempDir'
            ( \dirPath -> do
                let outputFile = dirPath <//> $(mkRelFile "out.out")
                hout <- openFile (toFilePath outputFile) WriteMode
                step "Evaluate"
                r' <- doEval hout tab (lookupFunInfo tab sym)
                hClose hout
                case r' of
                  Left _ -> assertBool "" True
                  Right _ -> assertFailure "no error"
            )
        Nothing -> assertFailure "no main function"
