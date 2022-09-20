module Asm where

import Asm.Run qualified as Run
import Base

allTests :: TestTree
allTests = testGroup "JuvixAsm tests" [Run.allTests]
