module Core where

import Base
import Core.Asm qualified as Asm
import Core.Compile qualified as Compile
import Core.Eval qualified as Eval
import Core.LetHoist qualified as LetHoist
import Core.Normalize qualified as Normalize
import Core.Print qualified as Print
import Core.Recursor qualified as Rec
import Core.Transformation qualified as Transformation

allTests :: TestTree
allTests =
  testGroup
    "JuvixCore tests"
    [ Rec.allTests,
      Eval.allTests,
      Print.allTests,
      Transformation.allTests,
      Asm.allTests,
      Compile.allTests,
      Normalize.allTests,
      LetHoist.allTests
    ]
