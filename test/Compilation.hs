module Compilation where

import Base
import Compilation.Negative qualified as N
import Compilation.Positive qualified as P
import Compilation.PositiveNew qualified as New

allTests :: TestTree
allTests = testGroup "Juvix compilation pipeline tests" [New.allTestsNoOptimize, P.allTestsNoOptimize, P.allTests, N.allTests]
