module Core.Asm.Positive where

import Base
import Core.Eval.Positive qualified as Eval
import Core.Asm.Base

allTests :: TestTree
allTests = testGroup "JuvixCore to JuvixAsm positive tests" (map liftTest (filterOutTests ignoredTests Eval.tests))

ignoredTests :: [String]
ignoredTests =
  [
    "Match with complex patterns"
  ]

liftTest :: Eval.PosTest -> TestTree
liftTest _testEval =
  fromTest
    Test
      { _testEval
      }

filterOutTests :: [String] -> [Eval.PosTest] -> [Eval.PosTest]
filterOutTests out = filter (\Eval.PosTest {..} -> _name `notElem` out)
