module VampIR where

import Base
import VampIR.Compilation.Positive qualified as PC
import VampIR.Core.Positive qualified as PT

allTests :: TestTree
allTests = testGroup "VampIR tests" [PT.allTests, PC.allTests]
