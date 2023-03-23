module Examples where

import Base
import Examples.Positive qualified as P

allTests :: TestTree
allTests = testGroup "Juvix compilation pipeline tests" [P.allTests]
