module Juvix.Compiler.Backend.Rust.Data.Result where

import Juvix.Prelude

newtype Result = Result
  { _resultRustCode :: Text
  }

makeLenses ''Result
