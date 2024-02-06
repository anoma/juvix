module Nockma.Compile where

import Base
import Nockma.Compile.Tree.Positive qualified as Tree

allTests :: TestTree
allTests = testGroup "Nockma compile" [Tree.allTests]
