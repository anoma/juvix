module Nockma where

import Base
import Nockma.Parse qualified as Parse

allTests :: TestTree
allTests = testGroup "Nockma tests" [Parse.allTests]
