module BackendGeb.Eval.Base where

import Base
import Data.Text.IO qualified as TIO
import Juvix.Compiler.Backend.Geb qualified as Geb
import Juvix.Compiler.Backend.Geb.Data.Context as Context
import Juvix.Compiler.Backend.Geb.Pretty.Values qualified as PrettyGeb
import Juvix.Prelude.Pretty

gebEvalAssertion ::
  Path Abs File ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
gebEvalAssertion mainFile expectedFile step = do
  step "Parse"
  input <- readFile (toFilePath mainFile)
  case Geb.runParser mainFile input of
    Left err -> assertFailure (show (pretty err))
    Right (Geb.ExpressionObject _) -> do
      step "No evalution for objects"
      assertFailure (unpack Geb.objNoEvalMsg)
    Right (Geb.ExpressionMorphism gebMorphism) -> do
      let env :: Geb.Env =
            Geb.Env
              { _envEvaluatorOptions = Geb.defaultEvaluatorOptions,
                _envContext = mempty
              }
      withTempDir' $
        \dirPath -> do
          let outputFile = dirPath <//> $(mkRelFile "out.out")
          step "Evaluate"
          hout <- openFile (toFilePath outputFile) WriteMode
          let result = Geb.eval' env gebMorphism
          case result of
            Left err -> do
              hClose hout
              assertFailure (show (pretty (fromJuvixError @GenericError err)))
            Right value -> do
              hPutStrLn hout (PrettyGeb.ppPrint value)
              hClose hout
              actualOutput <- TIO.readFile (toFilePath outputFile)
              expected <- TIO.readFile (toFilePath expectedFile)
              step "Compare expected and actual program output"
              assertEqDiffText
                ("Check: EVAL output = " <> toFilePath expectedFile)
                actualOutput
                expected

gebEvalErrorAssertion :: Path Abs File -> (String -> IO ()) -> Assertion
gebEvalErrorAssertion mainFile step = do
  step "Parse"
  input <- readFile (toFilePath mainFile)
  case Geb.runParser mainFile input of
    Left _ -> assertBool "" True
    Right (Geb.ExpressionObject _) -> assertFailure "no error"
    Right (Geb.ExpressionMorphism gebMorphism) -> do
      step "Evaluate"
      case Geb.eval' Geb.defaultEvalEnv gebMorphism of
        Left _ -> assertBool "" True
        Right _ -> assertFailure "no error"
