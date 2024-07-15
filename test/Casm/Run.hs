module Casm.Run where

import Base
import Casm.Run.Negative qualified as Negative
import Casm.Run.Positive qualified as Positive

allTests :: IO TestTree
allTests = testGroup "CASM run" <$> sequence [Positive.allTests, return Negative.allTests]
