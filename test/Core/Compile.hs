module Core.Compile where

import Base
import Core.Compile.Positive qualified as P

allTests :: TestTree
allTests = testGroup "JuvixCore compile" [P.allTests]
