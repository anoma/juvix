module Internal.Eval where

import Base
import Internal.Eval.Positive qualified as EvalP

allTests :: TestTree
allTests = testGroup "Internal to Core eval" [EvalP.allTests]
