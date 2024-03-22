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
      _optIgnoreHints = False,
      _optIgnoreTags = False
    }

data Options = Options
  { _optPrettyMode :: PrettyMode,
    _optIgnoreHints :: Bool,
    _optIgnoreTags :: Bool
  }

serializeOptions :: Options
serializeOptions =
  Options
    { _optPrettyMode = MinimizeDelimiters,
      _optIgnoreHints = True,
      _optIgnoreTags = True
    }

testOptions :: Options
testOptions =
  Options
    { _optPrettyMode = MinimizeDelimiters,
      _optIgnoreHints = False,
      _optIgnoreTags = True
    }

traceOptions :: Options
traceOptions =
  Options
    { _optPrettyMode = MinimizeDelimiters,
      _optIgnoreHints = False,
      _optIgnoreTags = False
    }

makeLenses ''Options

fromGenericOptions :: GenericOptions -> Options
fromGenericOptions GenericOptions {} = defaultOptions

instance CanonicalProjection GenericOptions Options where
  project = fromGenericOptions
