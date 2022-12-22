module Core.Asm where

import Base
import Core.Asm.Positive qualified as P

allTests :: TestTree
allTests = testGroup "JuvixCore to JuvixAsm translation" [P.allTests]
