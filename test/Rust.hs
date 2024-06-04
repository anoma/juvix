module Rust where

import Base
import Rust.Compilation qualified as Compilation
import Rust.RiscZero qualified as RiscZero

allTests :: TestTree
allTests = testGroup "Juvix to Rust tests" [Compilation.allTests, RiscZero.allTests]
