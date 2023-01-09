module Core.Compile.Positive where

import Base
import Core.Compile.Base
import Core.Eval.Positive qualified as Eval

allTests :: TestTree
allTests = testGroup "JuvixCore compilation tests" (map liftTest (Eval.filterOutTests ignoredTests Eval.tests))

-- Arbitrary precision integers and general pattern matching not yet supported
ignoredTests :: [String]
ignoredTests =
  [ "Tail recursion: Fibonacci numbers in linear time",
    "Fast exponentiation",
    "Nested 'case', 'let' and 'if' with variable capture",
    "Mutual recursion",
    "LetRec",
    "Big numbers",
    "Match with complex patterns"
  ]

liftTest :: Eval.PosTest -> TestTree
liftTest _testEval =
  fromTest
    Test
      { _testEval
      }
