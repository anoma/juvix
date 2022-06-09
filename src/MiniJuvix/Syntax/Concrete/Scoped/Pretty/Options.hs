module MiniJuvix.Syntax.Concrete.Scoped.Pretty.Options where

import MiniJuvix.Prelude

data Options = Options
  { _optShowNameIds :: Bool,
    _optInlineImports :: Bool,
    _optIndent :: Int
  }

defaultOptions :: Options
defaultOptions =
  Options
    { _optShowNameIds = False,
      _optInlineImports = False,
      _optIndent = 2
    }

makeLenses ''Options
