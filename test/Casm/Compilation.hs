module Casm.Compilation where

import Base
import Casm.Compilation.Negative qualified as Negative
import Casm.Compilation.Positive qualified as Positive

allTests :: IO TestTree
allTests =
  testGroup "Juvix to CASM compilation"
    <$> sequence
      [ Positive.allTests,
        Positive.allTestsNoOptimize,
        return Negative.allTests
      ]
