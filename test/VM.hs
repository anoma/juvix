module VM where

import Base
import VM.Run qualified as Run

allTests :: TestTree
allTests = testGroup "JuvixVM tests" [Run.allTests]
