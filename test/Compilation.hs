module Compilation where

import Base
import Compilation.Negative qualified as N
import Compilation.Positive qualified as P

allTests :: TestTree
allTests = testGroup "Juvix compilation pipeline tests" [P.allTests, N.allTests]
