module Casm.Reg where

import Base
import Casm.Reg.Positive qualified as P

allTests :: TestTree
allTests = testGroup "CASM from JuvixReg translation" [P.allTests]
