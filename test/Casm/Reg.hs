module Casm.Reg where

import Base
import Casm.Reg.Cairo qualified as Cairo
import Casm.Reg.Positive qualified as Positive

allTests :: IO TestTree
allTests =
  testGroup
    "JuvixReg to CASM translation"
    <$> sequence
      [ return Positive.allTests,
        Cairo.allTests
      ]
