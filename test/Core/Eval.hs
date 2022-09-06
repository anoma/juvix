module Core.Eval where

import Base
import Core.Eval.Negative qualified as EvalN
import Core.Eval.Positive qualified as EvalP

allTests :: TestTree
allTests = testGroup "JuvixCore eval" [EvalP.allTests, EvalN.allTests]
