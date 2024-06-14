module Reg.Transformation.CopyPropagation where

import Base
import Juvix.Compiler.Reg.Transformation
import Juvix.Compiler.Reg.Transformation.CopyPropagation
import Reg.Parse.Positive qualified as Parse
import Reg.Transformation.Base

allTests :: TestTree
allTests = testGroup "Copy Propagation" (map liftTest Parse.tests)

pipe :: [TransformationId]
pipe = [CopyPropagation]

liftTest :: Parse.PosTest -> TestTree
liftTest _testRun =
  fromTest
    Test
      { _testTransformations = pipe,
        _testAssertion = const (return ()),
        _testRun
      }
