module Reg.Parse where

import Base
import Reg.Parse.Positive qualified as P

allTests :: TestTree
allTests = testGroup "JuvixReg parsing" [P.allTests]
