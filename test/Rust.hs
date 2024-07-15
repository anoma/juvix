module Rust where

import Base
import Rust.Compilation qualified as Compilation
import Rust.RiscZero qualified as RiscZero

allTests :: IO TestTree
allTests =
  sequentialTestGroup "Juvix to Rust tests" AllFinish
    <$> sequence [Compilation.allTests, RiscZero.allTests]
