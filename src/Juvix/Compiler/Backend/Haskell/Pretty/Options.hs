module Juvix.Compiler.Backend.Haskell.Pretty.Options where

import Juvix.Prelude

newtype Options = Options
  { _optIndent :: Int
  }

defaultOptions :: Options
defaultOptions =
  Options
    { _optIndent = 2
    }

makeLenses ''Options
