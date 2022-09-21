module Asm.Run where

import Asm.Run.Positive qualified as RunP
import Asm.Run.Negative qualified as RunN
import Base

allTests :: TestTree
allTests = testGroup "JuvixAsm run" [RunP.allTests, RunN.allTests]
