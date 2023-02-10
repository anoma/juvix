module BackendGeb where

import BackendGeb.Eval qualified as Eval
import Base

allTests :: TestTree
allTests =
  testGroup
    "BackendGeb tests"
    [ Eval.allTests
    ]
