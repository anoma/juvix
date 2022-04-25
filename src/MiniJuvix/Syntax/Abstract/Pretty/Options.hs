module MiniJuvix.Syntax.Abstract.Pretty.Options where

import MiniJuvix.Prelude

data Options = Options
  { _optShowNameId :: Bool,
    _optIndent :: Int,
    _optShowDecreasingArgs :: ShowDecrArgs
  }

data ShowDecrArgs = OnlyArg | OnlyRel | ArgRel

defaultOptions :: Options
defaultOptions =
  Options
    { _optShowNameId = False,
      _optIndent = 2,
      _optShowDecreasingArgs = OnlyRel
    }

makeLenses ''Options
