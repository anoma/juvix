module Arity
  ( allTests,
  )
where

import Arity.Negative qualified as N
import Base

allTests :: TestTree
allTests = testGroup "Arity tests" [N.allTests]
