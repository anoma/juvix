module Anoma where

import Anoma.Compilation qualified as Compilation
import Base

allTests :: TestTree
allTests =
  testGroup
    "Anoma tests"
    [Compilation.allTests]
