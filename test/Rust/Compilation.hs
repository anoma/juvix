module Rust.Compilation where

import Base
import Rust.Compilation.Base
import Rust.Compilation.Positive qualified as P

allTests :: IO TestTree
allTests =
  withPrecondition precondition
    . return
    $ testGroup "Juvix to native Rust compilation tests" [P.allTests, P.allTestsNoOptimize]
