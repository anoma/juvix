module Rust.RiscZero where

import Base
import Rust.RiscZero.Base
import Rust.RiscZero.Positive qualified as P

allTests :: IO TestTree
allTests =
  withPrecondition precondition
    . return
    $ sequentialTestGroup "Juvix to RISC0 Rust compilation tests" AllFinish [P.allTests, P.allTestsNoOptimize]
