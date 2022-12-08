module Core.Transformation.RemoveTypeArgs (allTests) where

import Base
import Core.Eval.Positive qualified as Eval
import Core.Transformation.Base
import Juvix.Compiler.Core.Transformation

allTests :: TestTree
allTests = testGroup "Move applications and remove type arguments" (map liftTest Eval.tests)

pipe :: [TransformationId]
pipe = [LambdaLifting, MoveApps, RemoveTypeArgs]

liftTest :: Eval.PosTest -> TestTree
liftTest _testEval =
  fromTest
    Test
      { _testTransformations = pipe,
        _testAssertion = const (return ()),
        _testEval
      }
