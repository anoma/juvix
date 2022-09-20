module Asm.Run where

import Base
import Asm.Run.Positive qualified as RunP

allTests :: TestTree
allTests = testGroup "JuvixAsm run" [RunP.allTests]

