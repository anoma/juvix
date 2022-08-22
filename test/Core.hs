module Core where

import Core.Positive qualified as P
import Core.Negative qualified as N
import Base

allTests :: TestTree
allTests = testGroup "JuvixCore tests" [P.allTests, N.allTests]
