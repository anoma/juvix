module Nockma.Compile.Tree.Positive where

import Base
import Juvix.Compiler.Nockma.EvalCompiled
import Juvix.Compiler.Nockma.Evaluator qualified as NockmaEval
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Language qualified as Nockma
import Juvix.Compiler.Nockma.Pretty qualified as Nockma
import Juvix.Compiler.Nockma.Translation.FromTree
import Juvix.Compiler.Tree
import Nockma.Base
import Tree.Eval.Base
import Tree.Eval.Positive qualified as Tree

runNockmaAssertion :: Handle -> Symbol -> InfoTable -> IO ()
runNockmaAssertion hout _main tab = do
  anomaRes :: AnomaResult <-
    runM
      . runErrorIO' @JuvixError
      . runReader opts
      $ treeToAnoma' tab
  res <-
    runM
      . runOutputSem @(Term Natural)
        (embed . hPutStrLn hout . Nockma.ppPrint)
      . runReader NockmaEval.defaultEvalOptions
      $ evalCompiledNock' (anomaRes ^. anomaClosure) (anomaCall (anomaRes ^. anomaEnv) [])
  let ret = getReturn res
  whenJust ret (hPutStrLn hout . Nockma.ppPrint)
  where
    opts :: CompilerOptions
    opts =
      CompilerOptions
        { _compilerOptionsEnableTrace = True
        }

    getReturn :: Term Natural -> Maybe (Term Natural)
    getReturn = \case
      TermAtom Nockma.Atom {..}
        | _atomInfo ^. atomInfoHint == Just AtomHintVoid -> Nothing
      t -> Just t

testDescr :: Tree.PosTest -> TestDescr
testDescr Tree.PosTest {..} =
  let tRoot = Tree.root <//> _relDir
      file' = tRoot <//> _file
      expected' = tRoot <//> _expectedFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ treeEvalAssertionParam runNockmaAssertion file' expected' [] (const (return ()))
        }

-- | Tests which require Nockma-specific expected output files
testsConstr :: [Int]
testsConstr = [9, 28, 35]

testsNegativeInteger :: [Int]
testsNegativeInteger = [16]

-- Tests involving Strings and IO
testsUnsupported :: [Int]
testsUnsupported =
  [ 5,
    6,
    37
  ]

testsToIgnore :: [Int]
testsToIgnore = testsUnsupported ++ testsNegativeInteger

convertTest :: Tree.PosTest -> Maybe (Tree.PosTest)
convertTest p = do
  guard (testNum `notElem` map to3DigitString testsToIgnore)
  return $
    if
        | testNum `elem` map to3DigitString testsConstr -> over Tree.expectedFile go p
        | otherwise -> p
  where
    go :: Base.Path Rel File -> Base.Path Rel File
    go = replaceExtensions' [".nockma", ".out"]

    testNum :: String
    testNum = take 3 (drop 4 (p ^. Tree.name))
    to3DigitString :: Int -> String
    to3DigitString n
      | n < 10 = "00" ++ show n
      | n < 100 = "0" ++ show n
      | n < 1000 = show n
      | otherwise = impossible

allTests :: TestTree
allTests =
  testGroup
    "Nockma Tree compile positive tests"
    (map (mkTest . testDescr) (mapMaybe convertTest Tree.tests))
