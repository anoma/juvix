module Core where

import Base
import Core.Negative qualified as N
import Core.Positive qualified as P

allTests :: TestTree
allTests = testGroup "JuvixCore tests" [P.allTests, N.allTests]
