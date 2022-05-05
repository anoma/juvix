module BackendC where

import BackendC.Positive qualified as P
import Base

allTests :: TestTree
allTests = testGroup "Backend C tests" [P.allTests]
