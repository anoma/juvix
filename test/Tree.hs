module Tree where

import Base
import Tree.Asm qualified as Asm
import Tree.Eval qualified as Eval
import Tree.Transformation qualified as Transformation

allTests :: TestTree
allTests = testGroup "JuvixTree tests" [Eval.allTests, Asm.allTests, Transformation.allTests]
