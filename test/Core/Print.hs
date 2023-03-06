module Core.Print where

import Base
import Core.Print.Positive qualified as P

allTests :: TestTree
allTests = testGroup "JuvixCore print" [P.allTests]
