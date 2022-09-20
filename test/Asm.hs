module Asm where

import Base
import Asm.Run qualified as Run

allTests :: TestTree
allTests = testGroup "JuvixAsm tests" [Run.allTests]
