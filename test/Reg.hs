module Reg where

import Base
import Reg.Parse qualified as Parse
import Reg.Run qualified as Run

allTests :: TestTree
allTests = testGroup "JuvixReg tests" [Parse.allTests, Run.allTests]
