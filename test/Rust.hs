module Rust where

import Base
import Rust.Compilation qualified as Compilation

allTests :: IO TestTree
allTests =
  sequentialTestGroup "Juvix to Rust tests" AllFinish
    <$> sequence [Compilation.allTests]
