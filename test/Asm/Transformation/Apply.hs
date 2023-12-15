module Asm.Transformation.Apply (allTests) where

import Asm.Run.Positive qualified as Run
import Asm.Transformation.Base
import Base
import Juvix.Compiler.Asm.Options
import Juvix.Compiler.Asm.Transformation
import Juvix.Compiler.Asm.Transformation.Base

allTests :: TestTree
allTests =
  testGroup "Apply" $
    map liftTest $
      Run.filterTests
        [ "Test007: Higher-order functions",
          "Test022: Self-application",
          "Test025: Dynamic closure extension",
          "Test032: Church numerals"
        ]
        Run.tests

liftTest :: Run.PosTest -> TestTree
liftTest _testEval =
  fromTest
    Test
      { _testTransformation = runTransformation (runReader opts . computeApply),
        _testAssertion = \tab -> unless (checkNoCallClosures opts tab) (error "check apply"),
        _testEval
      }
  where
    opts =
      Options
        { _optDebug = True,
          _optLimits = getLimits TargetCWasm32Wasi True
        }
