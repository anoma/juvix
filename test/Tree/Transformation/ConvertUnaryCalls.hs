module Tree.Transformation.ConvertUnaryCalls (allTests) where

import Base
import Juvix.Compiler.Tree.Transformation
import Tree.Eval.Positive qualified as Eval
import Tree.Transformation.Base

allTests :: TestTree
allTests =
  testGroup
    "ConvertUnaryCalls"
    ( map liftTest
        $ Eval.filterTests
          [ "Test007: Higher-order functions",
            "Test022: Self-application",
            "Test025: Dynamic closure extension",
            "Test032: Church numerals",
            "Test043: Unary closure calls"
          ]
          Eval.tests
    )

pipe :: [TransformationId]
pipe = [ConvertUnaryCalls]

liftTest :: Eval.PosTest -> TestTree
liftTest _testEval =
  fromTest
    Test
      { _testTransformations = pipe,
        _testAssertion = const (return ()),
        _testEval
      }
