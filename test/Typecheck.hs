module Typecheck (allTests) where

import Base
import Typecheck.Negative qualified as N
import Typecheck.Positive qualified as P

allTests :: TestTree
allTests = testGroup "Type checker tests" [P.allTests, N.allTests]
