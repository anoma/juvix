module Core.Recursor where

import Base
import Core.Recursor.RMap qualified as RMap

allTests :: TestTree
allTests = testGroup "JuvixCore recursors" [RMap.allTests]
