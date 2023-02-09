module Geb.Eval where

import Base
import Geb.Eval.Negative qualified as EvalN
import Geb.Eval.Positive qualified as EvalP

allTests :: TestTree
allTests = testGroup "JuvixGeb eval" [EvalP.allTests, EvalN.allTests]
