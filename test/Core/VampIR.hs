module Core.VampIR where

import Base
import Core.VampIR.LetHoist qualified as LetHoist
import Core.VampIR.Positive qualified as P

allTests :: TestTree
allTests = testGroup "JuvixCore VampIR" [LetHoist.allTests, P.allTests]
