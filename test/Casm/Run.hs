module Casm.Run where

-- import Casm.Run.Negative qualified as RunN

import Base
import Casm.Run.Positive qualified as RunP

allTests :: TestTree
allTests = testGroup "CASM run" [RunP.allTests]
