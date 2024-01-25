module Nockma.Compile where

import Base
import Nockma.Compile.Positive qualified as P
import Nockma.Compile.Tree.Positive qualified as Tree

allTests :: TestTree
allTests = testGroup "Nockma compile" [P.allTests, Tree.allTests]
