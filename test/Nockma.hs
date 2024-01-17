module Nockma where

import Base
import Nockma.Compile qualified as Compile
import Nockma.Eval qualified as Eval
import Nockma.Parse qualified as Parse

allTests :: TestTree
allTests = testGroup "Nockma tests" [Parse.allTests, Eval.allTests, Compile.allTests]
