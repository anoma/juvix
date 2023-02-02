module Core.Transformation.Lifting (allTests) where

import Base
import Core.Eval.Positive qualified as Eval
import Core.Transformation.Base
import Juvix.Compiler.Core.Transformation

allTests :: TestTree
allTests =
  testGroup
    "Lifting"
    [ testGroup "Lambda and LetRec lifting" (map liftTest Eval.tests),
      testGroup "Only LetRec lifting" (map letRecLiftTest Eval.tests)
    ]

liftTest :: Eval.PosTest -> TestTree
liftTest _testEval =
  fromTest
    Test
      { _testTransformations = pipe,
        _testAssertion = \i -> unless (isLifted i) (error "not lambda lifted"),
        _testEval
      }
  where
    pipe :: [TransformationId]
    pipe = [LambdaLetRecLifting]

letRecLiftTest :: Eval.PosTest -> TestTree
letRecLiftTest _testEval =
  fromTest
    Test
      { _testTransformations = pipe,
        _testAssertion = \i -> unless (isLetRecLifted i) (error "not letrec lifted"),
        _testEval
      }
  where
    pipe :: [TransformationId]
    pipe = [LetRecLifting]
