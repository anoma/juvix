module Nockma.Compile.Tree.Positive where

import Base
import Juvix.Compiler.Asm.Options qualified as Asm
import Juvix.Compiler.Backend
import Juvix.Compiler.Nockma.EvalCompiled
import Juvix.Compiler.Nockma.Evaluator qualified as NockmaEval
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty qualified as Nockma
import Juvix.Compiler.Nockma.Translation.FromAsm qualified as Nockma
import Juvix.Compiler.Tree
import Tree.Eval.Base
import Tree.Eval.Positive qualified as Tree

runNockmaAssertion :: Handle -> Symbol -> InfoTable -> IO ()
runNockmaAssertion hout _main tab = do
  Nockma.Cell nockSubject nockMain <-
    runM
      . runReader
        (Asm.makeOptions TargetNockma True)
      . runReader
        (Nockma.CompilerOptions {_compilerOptionsEnableTrace = True})
      . runErrorIO' @JuvixError
      $ treeToNockma' tab
  res <-
    runM
      . runOutputSem @(Term Natural)
        (embed . hPutStrLn hout . Nockma.ppPrint)
      . runReader NockmaEval.defaultEvalOptions
      . evalCompiledNock' nockSubject
      $ nockMain
  let ret = getReturn res
  hPutStrLn hout (Nockma.ppPrint ret)
  where
    getReturn :: Term Natural -> Term Natural
    getReturn res =
      let valStack = getStack Nockma.ValueStack res
       in case valStack of
            TermCell c -> c ^. cellLeft
            TermAtom {} -> error "should be a cell"

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

testsSlow :: [Int]
testsSlow = [10, 11, 13, 17, 20, 23, 27, 28, 30, 32, 33, 34, 36]

testsAdt :: [Int]
testsAdt = [9, 15, 18, 25, 26, 29, 35]

testsNegativeInteger :: [Int]
testsNegativeInteger = [16, 31]

testsHopeless :: [Int]
testsHopeless =
  [ 5,
    6,
    14,
    24,
    37
  ]

testsBugged :: [Int]
testsBugged =
  []

testsToIgnore :: [Int]
testsToIgnore = testsHopeless ++ testsBugged ++ testsSlow ++ testsAdt ++ testsNegativeInteger

shouldRun :: Tree.PosTest -> Bool
shouldRun Tree.PosTest {..} = testNum `notElem` map to3DigitString testsToIgnore
  where
    testNum :: String
    testNum = take 3 (drop 4 _name)
    to3DigitString :: Int -> String
    to3DigitString n
      | n < 10 = "00" ++ show n
      | n < 100 = "0" ++ show n
      | n < 1000 = show n
      | otherwise = impossible

allTests :: TestTree
allTests =
  testGroup
    "Nockma Asm compile positive tests"
    (map (mkTest . testDescr) (filter shouldRun Tree.tests))
