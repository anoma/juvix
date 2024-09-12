module Tree.Parse where

import Base
import Tree.Parse.Positive qualified as P

allTests :: TestTree
allTests = testGroup "JuvixTree parsing" [P.allTests]
