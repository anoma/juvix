module TypeCheck (allTests) where

import Base
import TypeCheck.Negative qualified as N
import TypeCheck.Positive qualified as P

allTests :: TestTree
allTests = testGroup "Type checker tests" [P.allTests, N.allTests]
