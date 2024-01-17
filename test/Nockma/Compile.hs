module Nockma.Compile where

import Base
import Nockma.Compile.Asm.Positive qualified as Asm
import Nockma.Compile.Positive qualified as P

allTests :: TestTree
allTests = testGroup "Nockma compile" [P.allTests, Asm.allTests]
