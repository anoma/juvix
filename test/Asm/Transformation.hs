module Asm.Transformation where

import Asm.Transformation.Prealloc qualified as Prealloc
import Asm.Transformation.Reachability qualified as Reachability
import Base

allTests :: TestTree
allTests = testGroup "JuvixAsm transformations" [Prealloc.allTests, Reachability.allTests]
