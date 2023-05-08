module Core.Normalize where

import Base
import Core.Normalize.Positive qualified as P

allTests :: TestTree
allTests = testGroup "JuvixCore normalize" [P.allTests]
