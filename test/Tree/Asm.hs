module Tree.Asm where

import Base
import Tree.Asm.Positive qualified as P

allTests :: TestTree
allTests = testGroup "JuvixTree to JuvixAsm translation" [P.allTests]
