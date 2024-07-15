module Casm where

import Base
import Casm.Compilation qualified as Compile
import Casm.Reg qualified as Reg
import Casm.Run qualified as Run

allTests :: IO TestTree
allTests =
  testGroup "CASM tests"
    <$> sequence
      [ Run.allTests,
        Reg.allTests,
        Compile.allTests
      ]
