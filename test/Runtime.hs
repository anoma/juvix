module Runtime where

import Base
import Runtime.Positive qualified as P

allTests :: TestTree
allTests = testGroup "Runtime tests" [P.allTests]
