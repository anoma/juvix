module BackendGeb.FromCore where

import BackendGeb.FromCore.Positive qualified as FromCoreP
import Base

allTests :: TestTree
allTests =
  testGroup
    "Translation from Juvix Core to Geb"
    [FromCoreP.allTests]
