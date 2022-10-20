module Core.Transformation.Lifting (allTests) where

import Base
import Core.Eval.Positive qualified as Eval
import Core.Transformation.Base
import Juvix.Compiler.Core.Transformation

allTests :: TestTree
allTests = testGroup "Lambda lifting" (map liftTest Eval.tests)

pipe :: [TransformationId]
pipe = [LambdaLifting]

liftTest :: Eval.PosTest -> TestTree
liftTest _testEval =
  fromTest
    Test
      { _testTransformations = pipe,
        _testAssertion = \i -> unless (isLifted i) (error "not lambda lifted"),
        _testEval
      }
