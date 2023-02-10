module BackendGeb.Eval.Base where

import Base
import Data.Text.IO qualified as TIO
import Juvix.Compiler.Backend.Geb qualified as Geb
import Juvix.Parser.Error
import Juvix.Prelude

gebEvalAssertion ::
  Path Abs File ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
gebEvalAssertion mainFile expectedFile step = do
  step "Parse"
  input <- readFile (toFilePath mainFile)
  let r = Geb.runParser mainFile input
  case r of
    Left err -> assertFailure undefined --- TODO
    Right gebExpr -> do
      withTempDir' $
        \dirPath -> do
          let outputFile = dirPath <//> $(mkRelFile "out.out")
          hout <- openFile (toFilePath outputFile) WriteMode
          step "Evaluate"
          let r' =
                Geb.runEval
                  Geb.RunEvalArgs
                    { _runEvalArgsInputFile = mainFile,
                      _runEvalArgsContent = undefined, -- TODO input,
                      _runEvalArgsEvaluatorOptions = Geb.defaultEvaluatorOptions
                    }
          case r' of
            Left err -> do
              hClose hout
              undefined -- TODO
              -- assertFailure (show (pretty err))
            Right value -> do
              actualOutput <- TIO.readFile (toFilePath outputFile)
              expected <- TIO.readFile (toFilePath expectedFile)
              step "Compare expected and actual program output"
              assertEqDiff
                ("Check: EVAL output = " <> toFilePath expectedFile)
                actualOutput
                expected

-- unless
--   (Info.member kNoDisplayInfo (getInfo value))
--   (hPutStrLn hout (ppPrint value))
-- hClose hout

gebEvalErrorAssertion :: Path Abs File -> (String -> IO ()) -> Assertion
gebEvalErrorAssertion mainFile step = do
  undefined

-- step "Parse"
-- r <- parseFile mainFile
-- case r of
--   Left _ -> assertBool "" True
--   Right (_, Nothing) -> assertFailure "no error"
--   Right (tab, Just node) -> do
--     withTempDir'
--       ( \dirPath -> do
--           let outputFile = dirPath <//> $(mkRelFile "out.out")
--           hout <- openFile (toFilePath outputFile) WriteMode
--           step "Evaluate"
--           r' <- doEval mainFile hout tab node
--           hClose hout
--           case r' of
--             Left _ -> assertBool "" True
--             Right _ -> assertFailure "no error"
--       )