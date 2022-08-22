module Core.Base where

import Base
import Data.Text.IO qualified as TIO
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Error
import Juvix.Compiler.Core.Evaluator
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Language.Info qualified as Info
import Juvix.Compiler.Core.Language.Info.NoDisplayInfo
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Translation.FromSource
import System.IO.Extra (withTempDir)
import Text.Megaparsec.Pos qualified as M

coreEvalAssertion :: FilePath -> FilePath -> (String -> IO ()) -> Assertion
coreEvalAssertion mainFile expectedFile step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left err -> assertFailure (show (pretty err))
    Right (_, Nothing) -> do
      step "Compare expected and actual program output"
      expected <- TIO.readFile expectedFile
      assertEqDiff ("Check: EVAL output = " <> expectedFile) "" expected
    Right (tab, Just node) -> do
      withTempDir
        ( \dirPath -> do
            let outputFile = dirPath </> "out.out"
            hout <- openFile outputFile WriteMode
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
                actualOutput <- TIO.readFile outputFile
                step "Compare expected and actual program output"
                expected <- TIO.readFile expectedFile
                assertEqDiff ("Check: EVAL output = " <> expectedFile) actualOutput expected
        )

coreEvalErrorAssertion :: FilePath -> (String -> IO ()) -> Assertion
coreEvalErrorAssertion mainFile step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left _ -> assertBool "" True
    Right (_, Nothing) -> assertFailure "no error"
    Right (tab, Just node) -> do
      withTempDir
        ( \dirPath -> do
            let outputFile = dirPath </> "out.out"
            hout <- openFile outputFile WriteMode
            step "Evaluate"
            r' <- doEval mainFile hout tab node
            case r' of
              Left _ -> assertBool "" True
              Right _ -> assertFailure "no error"
        )

parseFile :: FilePath -> IO (Either ParserError (InfoTable, Maybe Node))
parseFile f = do
  s <- readFile f
  return $ runParser "" f emptyInfoTable s

doEval ::
  FilePath ->
  Handle ->
  InfoTable ->
  Node ->
  IO (Either CoreError Node)
doEval f hout tab node =
  catchEvalErrorIO defaultLoc (hEvalIO stdin hout (tab ^. identContext) [] node)
  where
    defaultLoc = singletonInterval (mkLoc f 0 (M.initialPos f))
