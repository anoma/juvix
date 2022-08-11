module Juvix.Compiler.Abstract.Pretty.Options where

import Juvix.Prelude

data Options = Options
  { _optShowNameIds :: Bool,
    _optShowDecreasingArgs :: ShowDecrArgs
  }

data ShowDecrArgs = OnlyArg | OnlyRel | ArgRel

defaultOptions :: Options
defaultOptions =
  Options
    { _optShowNameIds = False,
      _optShowDecreasingArgs = OnlyRel
    }

makeLenses ''Options
