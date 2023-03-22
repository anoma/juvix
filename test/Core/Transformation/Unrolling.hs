module Core.Transformation.Unrolling (allTests) where

import Base
import Core.Eval.Positive qualified as Eval
import Core.Transformation.Base
import Juvix.Compiler.Core.Data.IdentDependencyInfo
import Juvix.Compiler.Core.Transformation

allTests :: TestTree
allTests =
  testGroup
    "Recursion unrolling"
    ( map
        liftTest
        ( -- filter out tests which require recursion deeper than 140
          Eval.filterOutTests
            [ "Recursion",
              "Tail recursion",
              "Tail recursion: Fibonacci numbers in linear time",
              "Tail recursion through higher-order functions",
              "Recursive functions: McCarthy's 91 function, subtraction by increments",
              "Lists",
              "Streams without memoization",
              "Ackermann function (higher-order definition)",
              "Merge sort",
              "Big numbers"
            ]
            Eval.tests
        )
    )

pipe :: [TransformationId]
pipe = [UnrollRecursion]

liftTest :: Eval.PosTest -> TestTree
liftTest _testEval =
  fromTest
    Test
      { _testTransformations = pipe,
        _testAssertion = checkNoRecursion,
        _testEval
      }

checkNoRecursion :: InfoTable -> Assertion
checkNoRecursion tab
  | isCyclic (createIdentDependencyInfo tab) =
      assertFailure "recursion detected"
  | otherwise =
      return ()
