module Reg.Transformation.InitBranchVars where

import Base
import Juvix.Compiler.Reg.Transformation
import Juvix.Compiler.Reg.Transformation.InitBranchVars
import Juvix.Compiler.Reg.Transformation.SSA
import Reg.Parse.Positive qualified as Parse
import Reg.Transformation.Base

allTests :: TestTree
allTests = testGroup "InitBranchVars" (map liftTest $ Parse.filterOutTests ["Test039: Copy & constant propagation"] Parse.tests)

pipe :: [TransformationId]
pipe = [SSA, InitBranchVars]

liftTest :: Parse.PosTest -> TestTree
liftTest _testRun =
  fromTest
    Test
      { _testTransformations = pipe,
        _testAssertion = \tab -> do
          unless (checkSSA tab) $ error "check ssa"
          unless (checkInitialized tab) $ error "check initialized",
        _testRun
      }
