module Tree where

import Base
import Tree.Asm qualified as Asm

allTests :: TestTree
allTests = testGroup "JuvixTree tests" [Asm.allTests]
