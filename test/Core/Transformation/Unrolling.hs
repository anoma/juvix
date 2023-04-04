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
            [ "Test008: Recursion",
              "Test009: Tail recursion",
              "Test011: Tail recursion: Fibonacci numbers in linear time",
              "Test017: Tail recursion through higher-order functions",
              "Test020: McCarthy's 91 function, subtraction by increments",
              "Test023: Lists",
              "Test030: Streams without memoization",
              "Test032: Ackermann function (higher-order definition)",
              "Test035: Merge sort",
              "Test036: Big numbers"
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
checkNoRecursion tab =
  when
    (isCyclic (createIdentDependencyInfo tab))
    (assertFailure "recursion detected")
