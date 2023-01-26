module Core.Eval.Base where

import Base
import Data.Text.IO qualified as TIO
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Error
import Juvix.Compiler.Core.Evaluator
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.NoDisplayInfo
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Transformation
import Juvix.Compiler.Core.Translation.FromSource

coreEvalAssertion ::
  Path Abs File ->
  Path Abs File ->
  [TransformationId] ->
  (InfoTable -> Assertion) ->
  (String -> IO ()) ->
  Assertion
coreEvalAssertion mainFile expectedFile trans testTrans step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left err -> assertFailure (show (pretty err))
    Right (_, Nothing) -> do
      step "Compare expected and actual program output"
      expected <- TIO.readFile (toFilePath expectedFile)
      assertEqDiff ("Check: EVAL output = " <> toFilePath expectedFile) "" expected
    Right (tabIni, Just node) -> do
      let tab = applyTransformations trans (setupMainFunction tabIni node)
      testTrans tab
      let node' = fromJust $ tab ^. identContext . at (fromJust $ tab ^. infoMain)
      withTempDir'
        ( \dirPath -> do
            let outputFile = dirPath <//> $(mkRelFile "out.out")
            hout <- openFile (toFilePath outputFile) WriteMode
            step "Evaluate"
            r' <- doEval mainFile hout tab node'
            case r' of
              Left err -> do
                hClose hout
                assertFailure (show (pretty err))
              Right value -> do
                unless
                  (Info.member kNoDisplayInfo (getInfo value))
                  (hPutStrLn hout (ppPrint value))
                hClose hout
                actualOutput <- TIO.readFile (toFilePath outputFile)
                step "Compare expected and actual program output"
                expected <- TIO.readFile (toFilePath expectedFile)
                assertEqDiff ("Check: EVAL output = " <> toFilePath expectedFile) actualOutput expected
        )

coreEvalErrorAssertion :: Path Abs File -> (String -> IO ()) -> Assertion
coreEvalErrorAssertion mainFile step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left _ -> assertBool "" True
    Right (_, Nothing) -> assertFailure "no error"
    Right (tab, Just node) -> do
      withTempDir'
        ( \dirPath -> do
            let outputFile = dirPath <//> $(mkRelFile "out.out")
            hout <- openFile (toFilePath outputFile) WriteMode
            step "Evaluate"
            r' <- doEval mainFile hout tab node
            hClose hout
            case r' of
              Left _ -> assertBool "" True
              Right _ -> assertFailure "no error"
        )

parseFile :: Path Abs File -> IO (Either ParserError (InfoTable, Maybe Node))
parseFile f = do
  let f' = toFilePath f
  s <- readFile f'
  return $ runParser f emptyInfoTable s

doEval ::
  Path Abs File ->
  Handle ->
  InfoTable ->
  Node ->
  IO (Either CoreError Node)
doEval f hout tab node =
  catchEvalErrorIO defaultLoc (hEvalIO stdin hout (tab ^. identContext) [] node)
  where
    defaultLoc = singletonInterval (mkInitialLoc f)
