module BackendC where

import BackendC.Examples qualified as E
import BackendC.Positive qualified as P
import Base

allTests :: TestTree
allTests = testGroup "Backend C tests" [P.allTests, E.allTests]
