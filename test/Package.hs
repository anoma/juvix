module Package
  ( allTests,
  )
where

import Base
import Package.Negative qualified as N
import Package.Positive qualified as P

allTests :: TestTree
allTests = testGroup "Package loading tests" [N.allTests, P.allTests]
