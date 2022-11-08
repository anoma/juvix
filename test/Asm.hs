module Asm where

import Asm.Run qualified as Run
import Asm.Transformation qualified as Transformation
import Asm.Validate qualified as Validate
import Base

allTests :: TestTree
allTests = testGroup "JuvixAsm tests" [Validate.allTests, Run.allTests, Transformation.allTests]
