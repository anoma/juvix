module Core.Print.Positive where

import Base
import Core.Eval.Positive qualified as Eval
import Core.Print.Base

allTests :: TestTree
allTests = testGroup "JuvixCore parse and print positive tests" (map liftTest Eval.tests)

liftTest :: Eval.PosTest -> TestTree
liftTest _testEval =
  fromTest
    Test
      { _testEval
      }
