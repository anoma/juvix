module BackendGeb where

import BackendGeb.Compilation qualified as Compilation
import BackendGeb.Eval qualified as Eval
import BackendGeb.FromCore qualified as FromCore
import Base

allTests :: TestTree
allTests =
  testGroup
    "BackendGeb tests"
    [ Eval.allTests,
      FromCore.allTests,
      Compilation.allTests
    ]
