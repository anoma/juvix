module Reg.Transformation where

import Base
import Reg.Transformation.Identity qualified as Identity

allTests :: TestTree
allTests =
  testGroup
    "JuvixReg transformations"
    [ Identity.allTests
    ]
