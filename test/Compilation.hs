module Compilation where

import Base
import Compilation.Positive qualified as P

allTests :: TestTree
allTests = testGroup "Juvix compilation pipeline tests" [P.allTests]
