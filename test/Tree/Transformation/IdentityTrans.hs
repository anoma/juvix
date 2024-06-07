module Tree.Transformation.IdentityTrans (allTests) where

import Base
import Juvix.Compiler.Tree.Transformation
import Tree.Eval.Positive qualified as Eval
import Tree.Transformation.Base

allTests :: TestTree
allTests = testGroup "Identity" (map liftTest Eval.tests)

pipe :: [TransformationId]
pipe = [IdentityTrans, IdentityU, IdentityD]

liftTest :: Eval.PosTest -> TestTree
liftTest _testEval =
  fromTest
    Test
      { _testTransformations = pipe,
        _testAssertion = const (return ()),
        _testEval
      }
