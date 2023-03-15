module Core where

import Base
import Core.Asm qualified as Asm
import Core.Compile qualified as Compile
import Core.Eval qualified as Eval
import Core.Print qualified as Print
import Core.Transformation qualified as Transformation

allTests :: TestTree
allTests = testGroup "JuvixCore tests" [Eval.allTests, Print.allTests, Transformation.allTests, Asm.allTests, Compile.allTests]
