module Asm.Transformation where

import Asm.Transformation.Apply qualified as Apply
import Asm.Transformation.Prealloc qualified as Prealloc
import Base

allTests :: TestTree
allTests = testGroup "JuvixAsm transformations" [Prealloc.allTests, Apply.allTests]
