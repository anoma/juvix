module VM.Run where

import Base
import VM.Run.Positive qualified as RunP

allTests :: TestTree
allTests = testGroup "JuvixVM run" [RunP.allTests]
