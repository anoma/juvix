module Examples where

import Base
import Examples.Positive qualified as P

allTests :: TestTree
allTests = testGroup "Juvix milestone examples compilation tests" [P.allTests]
