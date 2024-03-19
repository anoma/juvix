module Casm.Reg where

import Base
import Casm.Reg.Cairo qualified as C
import Casm.Reg.Positive qualified as P

allTests :: TestTree
allTests = testGroup "JuvixReg to CASM translation" [P.allTests, C.allTests]
