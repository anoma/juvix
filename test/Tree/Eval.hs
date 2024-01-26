module Tree.Eval where

import Base
import Tree.Eval.Negative qualified as N
import Tree.Eval.Positive qualified as P

allTests :: TestTree
allTests = testGroup "JuvixTree evaluation" [P.allTests, N.allTests]
