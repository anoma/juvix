module MiniJuvix.Syntax.Abstract.Pretty.Options where

import MiniJuvix.Prelude

data Options = Options
  { _optShowNameIds :: Bool,
    _optIndent :: Int,
    _optShowDecreasingArgs :: ShowDecrArgs
  }

data ShowDecrArgs = OnlyArg | OnlyRel | ArgRel

defaultOptions :: Options
defaultOptions =
  Options
    { _optShowNameIds = False,
      _optIndent = 2,
      _optShowDecreasingArgs = OnlyRel
    }

makeLenses ''Options
