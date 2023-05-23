module Core.Transformation.Pipeline (allTests) where

import Base
import Core.Eval.Positive qualified as Eval
import Core.Transformation.Base
import Juvix.Compiler.Core.Transformation

allTests :: TestTree
allTests = testGroup "Transformation pipeline (to Stripped)" (map liftTest Eval.compilableTests)

pipe :: [TransformationId]
pipe = toStrippedTransformations

liftTest :: Eval.PosTest -> TestTree
liftTest _testEval =
  fromTest
    Test
      { _testTransformations = pipe,
        _testAssertion = const (return ()),
        _testEval
      }
