module Asm.Validate where

import Asm.Validate.Negative qualified as ValidateN
import Base

allTests :: TestTree
allTests = testGroup "JuvixAsm validate" [ValidateN.allTests]
