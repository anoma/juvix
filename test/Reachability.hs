module Reachability
  ( allTests,
  )
where

import Base
import Reachability.Positive qualified as P

allTests :: TestTree
allTests = testGroup "Reachability tests" [P.allTests]
