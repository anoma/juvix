module Reg.Transformation.ConstantPropagation where

import Base
import Juvix.Compiler.Reg.Transformation
import Reg.Parse.Positive qualified as Parse
import Reg.Transformation.Base

allTests :: TestTree
allTests = testGroup "JuvixReg Constant Propagation" (map liftTest Parse.tests)

pipe :: [TransformationId]
pipe = [ConstantPropagation]

liftTest :: Parse.PosTest -> TestTree
liftTest _testRun =
  fromTest
    Test
      { _testTransformations = pipe,
        _testAssertion = const (return ()),
        _testRun
      }
