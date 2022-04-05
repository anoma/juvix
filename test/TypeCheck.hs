module TypeCheck (allTests) where

import Base
import TypeCheck.Negative qualified as N

allTests :: TestTree
allTests = testGroup "TypeCheck tests" [N.allTests]
