module Anoma where

import Anoma.Client qualified as Client
import Anoma.Compilation qualified as Compilation
import Base

allTests :: TestTree
allTests =
  testGroup
    "Anoma tests"
    [ Compilation.allTests
    -- Client.allTests
    ]
