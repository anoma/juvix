module Scope (
  allTests
             ) where

import Base
import qualified Scope.Negative as N
import qualified Scope.Positive as P

allTests :: TestTree
allTests = testGroup "Scope tests" [P.allTests, N.allTests]
