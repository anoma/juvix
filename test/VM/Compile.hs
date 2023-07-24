module VM.Compile where

import Base
import VM.Compile.Positive qualified as CompileP

allTests :: TestTree
allTests = testGroup "JuvixVM compile" [CompileP.allTests]
