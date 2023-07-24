module VM where

import Base
import VM.Compile qualified as Compile
import VM.Run qualified as Run

allTests :: TestTree
allTests = testGroup "JuvixVM tests" [Run.allTests, Compile.allTests]
