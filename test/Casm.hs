module Casm where

import Base
import Casm.Run qualified as Run

allTests :: TestTree
allTests = testGroup "CASM tests" [Run.allTests]
