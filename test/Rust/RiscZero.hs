module Rust.RiscZero where

import Base
import Rust.RiscZero.Positive qualified as P

allTests :: TestTree
allTests = sequentialTestGroup "Juvix to RISC0 Rust compilation tests" AllFinish [P.allTests, P.allTestsNoOptimize]
