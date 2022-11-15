module Asm.Compile where

import Asm.Compile.Positive qualified as P
import Base

allTests :: TestTree
allTests = testGroup "JuvixAsm compile" [P.allTests]
