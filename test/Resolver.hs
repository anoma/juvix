module Resolver
  ( allTests,
  )
where

import Base
import Resolver.Negative qualified as N

allTests :: TestTree
allTests = testGroup "Path resolver tests" [N.allTests]
