module Core.Eval.Base where

import Base
import Data.HashMap.Strict qualified as HashMap
import Data.Text.IO qualified as TIO
import GHC.Base (seq)
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Error
import Juvix.Compiler.Core.Evaluator
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.NoDisplayInfo
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Transformation
import Juvix.Compiler.Core.Translation.FromSource

coreEvalAssertion' ::
  InfoTable ->
  Path Abs File ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
coreEvalAssertion' tab mainFile expectedFile step =
  length (fromText (ppPrint tab) :: String) `seq`
    case (tab ^. infoMain) >>= ((tab ^. identContext) HashMap.!?) of
      Just node -> do
        withTempDir'
          ( \dirPath -> do
              let outputFile = dirPath <//> $(mkRelFile "out.out")
              hout <- openFile (toFilePath outputFile) WriteMode
              step "Evaluate"
              r' <- doEval mainFile hout tab node
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
                  assertEqDiffText ("Check: EVAL output = " <> toFilePath expectedFile) actualOutput expected
          )
      Nothing -> assertFailure ("No main function registered in: " <> toFilePath mainFile)

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
      assertEqDiffText ("Check: EVAL output = " <> toFilePath expectedFile) "" expected
    Right (tabIni, Just node) ->
      case run $ runReader defaultCoreOptions $ runError $ applyTransformations trans (setupMainFunction tabIni node) of
        Left err -> assertFailure (show (pretty (fromJuvixError @GenericError err)))
        Right tab -> do
          testTrans tab
          coreEvalAssertion' tab mainFile expectedFile step

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

parseFile :: Path Abs File -> IO (Either MegaparsecError (InfoTable, Maybe Node))
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
  catchEvalErrorIO defaultLoc (hEvalIO hout stdin hout (tab ^. identContext) [] node)
  where
    defaultLoc = singletonInterval (mkInitialLoc f)
