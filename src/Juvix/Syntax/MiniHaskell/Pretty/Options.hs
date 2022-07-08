module Juvix.Syntax.MiniHaskell.Pretty.Options where

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
