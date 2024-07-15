module VampIR where

import Base
import VampIR.Compilation.Negative qualified as Negative
import VampIR.Compilation.Positive qualified as CompilationPositive
import VampIR.Core.Base
import VampIR.Core.Positive qualified as CorePositive

allTests :: IO TestTree
allTests =
  withPrecondition precondition
    . return
    $ testGroup
      "VampIR tests"
      [ CorePositive.allTests,
        CompilationPositive.allTests,
        Negative.allTests
      ]
