module Nockma.Parse where

import Base
import Nockma.Parse.Positive qualified as P

allTests :: TestTree
allTests = testGroup "Nockma parse" [P.allTests]
