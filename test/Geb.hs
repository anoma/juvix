module Geb where

import Base
import Geb.Compilation.Negative qualified as N
import Geb.Compilation.Positive qualified as PC
import Geb.Core.Positive qualified as PT

allTests :: TestTree
allTests = testGroup "Geb tests" [PT.allTests, PC.allTests, N.allTests]