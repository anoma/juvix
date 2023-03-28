module Formatter where

import Base
import Formatter.Positive qualified as P

allTests :: TestTree
allTests = testGroup "Formatter tests" [P.allTests]
