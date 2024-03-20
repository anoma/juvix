module Casm.Run where

import Base
import Casm.Run.Negative qualified as RunN
import Casm.Run.Positive qualified as RunP

allTests :: TestTree
allTests = testGroup "CASM run" [RunP.allTests, RunN.allTests]
