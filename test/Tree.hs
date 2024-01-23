module Tree where

import Base
import Tree.Asm qualified as Asm
import Tree.Eval qualified as Eval

allTests :: TestTree
allTests = testGroup "JuvixTree tests" [Eval.allTests, Asm.allTests]
