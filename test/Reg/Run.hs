module Reg.Run where

import Base
import Reg.Run.Positive qualified as RunP

allTests :: TestTree
allTests = testGroup "JuvixReg run" [RunP.allTests]
