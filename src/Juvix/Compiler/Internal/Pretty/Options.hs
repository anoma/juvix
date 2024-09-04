module Juvix.Compiler.Internal.Pretty.Options where

import Juvix.Prelude
import Prelude qualified

data ShowDecrArgs
  = OnlyArg
  | OnlyRel
  | ArgRel
  deriving stock (Enum, Bounded, Data)

instance Show ShowDecrArgs where
  show = \case
    OnlyArg -> "arg"
    OnlyRel -> "rel"
    ArgRel -> "both"

showDecrArgsHelp :: (IsString str) => ShowDecrArgs -> str
showDecrArgsHelp = \case
  OnlyRel -> "Show only the size relation"
  OnlyArg -> "Show only the argument"
  ArgRel -> "Show both the argument and the size relation"

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
