module Core.Compile.Positive where

import Base
import Core.Compile.Base
import Core.Eval.Positive qualified as Eval

allTests :: TestTree
allTests = testGroup "JuvixCore compilation tests" (map liftTest (Eval.filterOutTests ignoredTests Eval.tests))

-- Arbitrary precision integers not yet supported
ignoredTests :: [String]
ignoredTests =
  [ "Test011: Tail recursion: Fibonacci numbers in linear time",
    "Test022: Fast exponentiation",
    "Test025: Mutual recursion",
    "Test026: Nested 'case', 'let' and 'if' with variable capture",
    "Test034: Evaluation order",
    "Test036: Big numbers",
    "Test040: LetRec - fib, fact"
  ]

liftTest :: Eval.PosTest -> TestTree
liftTest _testEval =
  fromTest
    Test
      { _testEval
      }
