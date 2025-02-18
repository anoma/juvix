module Nockma.Eval where

import Base
import Nockma.Eval.Negative qualified as N
import Nockma.Eval.Positive qualified as P

allTests :: TestTree
allTests = testGroup "Nockma eval" [P.allTests, N.allTests]
