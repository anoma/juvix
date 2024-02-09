module Reg where

import Base
import Reg.Parse qualified as Parse

allTests :: TestTree
allTests = testGroup "JuvixReg tests" [Parse.allTests]
