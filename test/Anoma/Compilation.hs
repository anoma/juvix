module Anoma.Compilation where

import Anoma.Compilation.Negative qualified as N
import Anoma.Compilation.Positive qualified as P
import Base

allTests :: TestTree
allTests = sequentialTestGroup "Compilation to Anoma" AllFinish [P.allTests, N.allTests]
