module Tree.Transformation.Apply (allTests) where

import Base
import Juvix.Compiler.Tree.Transformation
import Juvix.Compiler.Tree.Transformation.Apply (checkNoCallClosures)
import Tree.Eval.Positive qualified as Eval
import Tree.Transformation.Base

allTests :: TestTree
allTests =
  testGroup
    "Apply"
    ( map liftTest $
        Eval.filterTests
          [ "Test007: Higher-order functions",
            "Test022: Self-application",
            "Test025: Dynamic closure extension",
            "Test032: Church numerals"
          ]
          Eval.tests
    )

pipe :: [TransformationId]
pipe = [Apply]

liftTest :: Eval.PosTest -> TestTree
liftTest _testEval =
  fromTest
    Test
      { _testTransformations = pipe,
        _testAssertion = assertBool "check no CallClosures" . checkNoCallClosures,
        _testEval
      }
