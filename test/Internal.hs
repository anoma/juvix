module Internal where

import Base
import Internal.Eval qualified as Eval

allTests :: TestTree
allTests = testGroup "Internal to Core tests" [Eval.allTests]
