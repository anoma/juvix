module Asm.Run where

import Asm.Run.Positive qualified as RunP
import Base

allTests :: TestTree
allTests = testGroup "JuvixAsm run" [RunP.allTests]
