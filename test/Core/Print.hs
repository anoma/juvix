module Core.Print where

import Base
import Core.Print.Negative qualified as N
import Core.Print.Positive qualified as P

allTests :: TestTree
allTests = testGroup "JuvixCore parse and print" [P.allTests, N.allTests]
