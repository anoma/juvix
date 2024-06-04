module Rust.RiscZero where

import Base
import Rust.RiscZero.Positive qualified as P

allTests :: TestTree
allTests = testGroup "Juvix to RISC0 Rust compilation tests" [P.allTests, P.allTestsNoOptimize]
