module Core where

import Core.Positive qualified as P
import Base

allTests :: TestTree
allTests = testGroup "JuvixCore tests" [P.allTests]
