module Juvix.Compiler.Nockma.Pretty.Options where

import Juvix.Prelude

data PrettyMode
  = MinimizeDelimiters
  | AllDelimiters
  deriving stock (Data)

defaultOptions :: Options
defaultOptions =
  Options
    { _optPrettyMode = MinimizeDelimiters,
      _optIgnoreHints = False
    }

data Options = Options
  { _optPrettyMode :: PrettyMode,
    _optIgnoreHints :: Bool
  }

serializeOptions :: Options
serializeOptions =
  Options
    { _optPrettyMode = MinimizeDelimiters,
      _optIgnoreHints = True
    }

traceOptions :: Options
traceOptions =
  Options
    { _optPrettyMode = MinimizeDelimiters,
      _optIgnoreHints = False
    }

makeLenses ''Options

fromGenericOptions :: GenericOptions -> Options
fromGenericOptions GenericOptions {} = defaultOptions

instance CanonicalProjection GenericOptions Options where
  project = fromGenericOptions
