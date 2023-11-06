module Typecheck (allTests) where

import Base
import Typecheck.Negative qualified as N
import Typecheck.Positive qualified as P
import Typecheck.PositiveNew qualified as New

allTests :: TestTree
allTests = testGroup "Type checker tests" [New.allTests, P.allTests, N.allTests]
