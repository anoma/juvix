module Juvix.Compiler.Backend.C.Data.Types where

import Juvix.Prelude

newtype MiniCResult = MiniCResult
  { _resultCCode :: Text
  }
