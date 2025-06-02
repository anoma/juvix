module Anoma.Client where

import Anoma.Client.Positive qualified as P
import Base

allTests :: TestTree
allTests = testGroup "Execution with the Anoma client" [P.allTests]
