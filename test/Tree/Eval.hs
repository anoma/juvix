module Tree.Eval where

import Base
import Tree.Eval.Positive qualified as P

allTests :: TestTree
allTests = testGroup "JuvixTree evaluation" [P.allTests]
