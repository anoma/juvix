module MiniJuvix.Syntax.MonoJuvix.Pretty.Options where

import MiniJuvix.Prelude

newtype Options = Options
  { _optIndent :: Int
  }

defaultOptions :: Options
defaultOptions =
  Options
    { _optIndent = 2
    }

makeLenses ''Options
