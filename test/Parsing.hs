module Parsing
  ( allTests,
  )
where

import Base
import Parsing.Negative qualified as N

allTests :: TestTree
allTests = testGroup "Parsing tests" [N.allTests]
