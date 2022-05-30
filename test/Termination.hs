module Termination (allTests) where

import Base
import Termination.Negative qualified as N
import Termination.Positive qualified as P

allTests :: TestTree
allTests = testGroup "Termination checker tests" [P.allTests, N.allTests]
