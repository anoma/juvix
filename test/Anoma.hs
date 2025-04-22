module Anoma where

import Anoma.Client qualified as Client
import Anoma.Compilation qualified as Compilation
import Base

allTests :: TestTree
allTests =
  sequentialTestGroup
    "Anoma tests"
    AllFinish
    [ Compilation.allTests,
      Client.allTests
    ]
