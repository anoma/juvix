module BackendGeb.Compilation where

import BackendGeb.Compilation.Positive qualified as P
import Base

allTests :: TestTree
allTests =
  testGroup
    "Compilation to Geb"
    [P.allTests]
