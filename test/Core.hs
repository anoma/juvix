module Core where

import Base
import Core.Eval qualified as Eval
import Core.Transformation qualified as Transformation

allTests :: TestTree
allTests = testGroup "JuvixCore tests" [Eval.allTests, Transformation.allTests]
