module Rust where

import Base
import Rust.Compilation qualified as Compilation
import Rust.RiscZero qualified as RiscZero

allTests :: TestTree
allTests = sequentialTestGroup "Juvix to Rust tests" AllFinish [Compilation.allTests, RiscZero.allTests]
