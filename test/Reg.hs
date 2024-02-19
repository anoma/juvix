module Reg where

import Base
import Reg.Parse qualified as Parse
import Reg.Run qualified as Run
import Reg.Transformation qualified as Transformation

allTests :: TestTree
allTests = testGroup "JuvixReg tests" [Parse.allTests, Run.allTests, Transformation.allTests]
