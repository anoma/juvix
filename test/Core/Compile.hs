module Core.Compile where

import Core.Compile.Positive qualified as P
import Base

allTests :: TestTree
allTests = testGroup "JuvixCore compile" [P.allTests]
