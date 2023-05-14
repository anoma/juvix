module Core.LetHoist where

import Base
import Core.LetHoist.Positive qualified as P

allTests :: TestTree
allTests = testGroup "JuvixCore let-hoist" [P.allTests]
