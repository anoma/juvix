module Casm where

import Base
import Casm.Compilation qualified as Compile
import Casm.Reg qualified as Reg
import Casm.Run qualified as Run

allTests :: TestTree
allTests = testGroup "CASM tests" [Run.allTests, Reg.allTests, Compile.allTests]
