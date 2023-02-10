module BackendGeb.Eval where

import BackendGeb.Eval.Negative qualified as EvalN
import BackendGeb.Eval.Positive qualified as EvalP
import Base

allTests :: TestTree
allTests =
  testGroup
    "JuvixGeb eval"
    [EvalP.allTests, EvalN.allTests]
