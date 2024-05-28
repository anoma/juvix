module Rust where

import Base
import Rust.Compilation qualified as Compilation

allTests :: TestTree
allTests = testGroup "Juvix to Rust tests" [Compilation.allTests]
