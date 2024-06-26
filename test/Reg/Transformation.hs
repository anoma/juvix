module Reg.Transformation where

import Base
import Reg.Transformation.ConstantPropagation qualified as ConstantPropagation
import Reg.Transformation.CopyPropagation qualified as CopyPropagation
import Reg.Transformation.IdentityTrans qualified as IdentityTrans
import Reg.Transformation.InitBranchVars qualified as InitBranchVars
import Reg.Transformation.SSA qualified as SSA

allTests :: TestTree
allTests =
  testGroup
    "JuvixReg transformations"
    [ IdentityTrans.allTests,
      SSA.allTests,
      InitBranchVars.allTests,
      CopyPropagation.allTests,
      ConstantPropagation.allTests
    ]
