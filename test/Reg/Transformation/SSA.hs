module Reg.Transformation.SSA where

import Base
import Juvix.Compiler.Reg.Transformation
import Juvix.Compiler.Reg.Transformation.SSA
import Reg.Parse.Positive qualified as Parse
import Reg.Transformation.Base

allTests :: TestTree
allTests = testGroup "SSA" (map liftTest Parse.tests)

pipe :: [TransformationId]
pipe = [SSA]

liftTest :: Parse.PosTest -> TestTree
liftTest _testRun =
  fromTest
    Test
      { _testTransformations = pipe,
        _testAssertion = \tab -> unless (checkSSA tab) $ error "check SSA",
        _testRun
      }
