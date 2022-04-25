module MiniJuvix.Syntax.Concrete.Scoped.Pretty.Options where

import MiniJuvix.Prelude

data Options = Options
  { _optShowNameId :: Bool,
    _optInlineImports :: Bool,
    _optIndent :: Int
  }

defaultOptions :: Options
defaultOptions =
  Options
    { _optShowNameId = False,
      _optInlineImports = False,
      _optIndent = 2
    }

makeLenses ''Options
