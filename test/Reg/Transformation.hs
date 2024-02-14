module Reg.Transformation where

import Base
import Reg.Transformation.Identity qualified as Identity
import Reg.Transformation.InitBranchVars qualified as InitBranchVars
import Reg.Transformation.SSA qualified as SSA

allTests :: TestTree
allTests =
  testGroup
    "JuvixReg transformations"
    [ Identity.allTests,
      SSA.allTests,
      InitBranchVars.allTests
    ]
