module Anoma.Compilation where

import Anoma.Compilation.Positive qualified as P
import Base

allTests :: TestTree
allTests = testGroup "Compilation to Anoma" [P.allTests]
