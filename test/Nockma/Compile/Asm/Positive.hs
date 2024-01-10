module Nockma.Compile.Asm.Positive where

import Asm.Run.Base
import Asm.Run.Positive qualified as Asm
import Base
import Juvix.Compiler.Asm
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty qualified as Nockma
import Juvix.Compiler.Nockma.Translation.FromAsm
import Juvix.Compiler.Nockma.Translation.FromAsm qualified as Nockma

runNockmaAssertion :: Handle -> Symbol -> InfoTable -> IO ()
runNockmaAssertion hout sym tab = do
  tab' <- runM $ runErrorIO' @AsmError (computeApply tab)
  let Nockma.Cell nockSubject nockMain = Nockma.fromAsm sym tab'
  res <- runM $ runOutputSem @(Term Natural) (embed . hPutStrLn hout . Nockma.ppPrint) (evalCompiledNock' nockSubject nockMain)
  let ret = getReturn res
  hPutStrLn hout (Nockma.ppPrint ret)
  where
    getReturn :: Term Natural -> Term Natural
    getReturn res =
      let valStack = getStack ValueStack res
       in case valStack of
            TermCell c -> c ^. cellLeft
            TermAtom {} -> error "should be a cell"

testDescr :: Asm.PosTest -> TestDescr
testDescr Asm.PosTest {..} =
  let tRoot = Asm.root <//> _relDir
      file' = tRoot <//> _file
      expected' = tRoot <//> _expectedFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ asmRunAssertionParam runNockmaAssertion file' expected' return (const (return ()))
        }

testsToRun :: [String]
testsToRun =
  [ "Test001: Arithmetic opcodes",
    "Test002: Direct call",
    "Test003: Indirect call",
    "Test004: Tail calls"
  ]

testsSlow :: [Int]
testsSlow = [10, 13, 17, 20, 23, 27, 28, 30, 33, 34, 36]

testsAdt :: [Int]
testsAdt = [15, 18, 26, 29, 35]

testsHopeless :: [Int]
testsHopeless =
  [ 5,
    6,
    9,
    11,
    14,
    24,
    37
  ]

testsBugged :: [Int]
testsBugged =
  [ 12,
    7,
    16,
    21,
    22,
    25,
    31,
    32,
    38
  ]

testsToIgnore :: [Int]
testsToIgnore = testsHopeless ++ testsBugged ++ testsSlow ++ testsAdt

shouldRun :: Asm.PosTest -> Bool
shouldRun Asm.PosTest {..} = testNum `notElem` map to3DigitString testsToIgnore
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
    "Nockma Asm eval positive tests"
    -- (map (mkTest . testDescr) (Asm.filterTests testsToRun Asm.tests))
    -- (map (mkTest . testDescr) (Asm.tests))
    (map (mkTest . testDescr) (filter shouldRun Asm.tests))
