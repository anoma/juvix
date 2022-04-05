module Scope
  ( allTests,
  )
where

import Base
import Scope.Negative qualified as N
import Scope.Positive qualified as P

allTests :: TestTree
allTests = testGroup "Scope tests" [P.allTests, N.allTests]
