module Core.Transformation.IdentityTrans (allTests) where

import Base
import Core.Eval.Positive qualified as Eval
import Core.Transformation.Base
import Juvix.Compiler.Core.Transformation

allTests :: TestTree
allTests = testGroup "Identity" (map liftTest Eval.tests)

pipe :: [TransformationId]
pipe = [IdentityTrans]

liftTest :: Eval.PosTest -> TestTree
liftTest _testEval =
  fromTest
    Test
      { _testTransformations = pipe,
        _testAssertion = const (return ()),
        _testEval
      }
