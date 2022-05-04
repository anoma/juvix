module MonoJuvix (allTests) where

import Base
import MonoJuvix.Positive qualified as P

allTests :: TestTree
allTests = testGroup "MonoJuvix tests" [P.allTests]
