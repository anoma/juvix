module Rust.Compilation where

import Base
import Rust.Compilation.Positive qualified as P

allTests :: TestTree
allTests = testGroup "Juvix to native Rust compilation tests" [P.allTests, P.allTestsNoOptimize]
