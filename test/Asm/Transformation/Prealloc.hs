module Asm.Transformation.Prealloc (allTests) where

import Asm.Run.Positive qualified as Run
import Asm.Transformation.Base
import Base
import Juvix.Compiler.Asm.Options
import Juvix.Compiler.Asm.Transformation
import Juvix.Compiler.Asm.Transformation.Base

allTests :: TestTree
allTests = testGroup "Prealloc" (map liftTest Run.tests)

liftTest :: Run.PosTest -> TestTree
liftTest _testEval =
  fromTest
    Test
      { _testTransformation = runTransformation (runReader opts . computePrealloc),
        _testAssertion = \tab -> unless (checkPrealloc opts tab) (error "check prealloc"),
        _testEval
      }
  where
    opts =
      Options
        { _optDebug = True,
          _optLimits = getLimits TargetCWasm32Wasi False
        }
