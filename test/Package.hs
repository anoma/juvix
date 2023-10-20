module Package
  ( allTests,
  )
where

import Base
import Package.Negative qualified as N

allTests :: TestTree
allTests = testGroup "Package loading tests" [N.allTests]
