module TypeCheck (allTests) where

import Base
import qualified TypeCheck.Negative as N

allTests :: TestTree
allTests = testGroup "TypeCheck tests" [N.allTests]
