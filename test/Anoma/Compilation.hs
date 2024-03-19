module Anoma.Compilation where

import Anoma.Compilation.Negative qualified as N
import Anoma.Compilation.Positive qualified as P
import Base

allTests :: TestTree
allTests = testGroup "Compilation to Anoma" [P.allTests, N.allTests]
