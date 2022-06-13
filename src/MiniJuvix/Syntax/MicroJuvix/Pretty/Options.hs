module MiniJuvix.Syntax.MicroJuvix.Pretty.Options where

import MiniJuvix.Prelude

data Options = Options
  { _optIndent :: Int,
    _optShowNameIds :: Bool
  }

defaultOptions :: Options
defaultOptions =
  Options
    { _optIndent = 2,
      _optShowNameIds = False
    }

makeLenses ''Options
