module Juvix.Compiler.Internal.Pretty.Options where

import Juvix.Prelude

data ShowDecrArgs
  = OnlyArg
  | OnlyRel
  | ArgRel
  deriving stock (Data)

defaultOptions :: Options
defaultOptions =
  Options
    { _optShowNameIds = False,
      _optShowDecreasingArgs = OnlyRel
    }

data Options = Options
  { _optShowNameIds :: Bool,
    _optShowDecreasingArgs :: ShowDecrArgs
  }

traceOptions :: Options
traceOptions =
  Options
    { _optShowNameIds = True,
      _optShowDecreasingArgs = ArgRel
    }

makeLenses ''Options

fromGenericOptions :: GenericOptions -> Options
fromGenericOptions GenericOptions {..} =
  defaultOptions
    { _optShowNameIds = _showNameIds
    }

instance CanonicalProjection GenericOptions Options where
  project = fromGenericOptions
