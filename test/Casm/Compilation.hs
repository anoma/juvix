module Casm.Compilation where

import Base
import Casm.Compilation.Negative qualified as N
import Casm.Compilation.Positive qualified as P

allTests :: TestTree
allTests = testGroup "Juvix to CASM compilation" [P.allTests, P.allTestsNoOptimize, N.allTests]
