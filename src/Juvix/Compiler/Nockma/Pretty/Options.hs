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
      _optIgnoreTags = False,
      _optNock = False
    }

data Options = Options
  { _optPrettyMode :: PrettyMode,
    _optIgnoreHints :: Bool,
    _optIgnoreTags :: Bool,
    _optNock :: Bool
  }

serializeOptions :: Options
serializeOptions =
  Options
    { _optPrettyMode = MinimizeDelimiters,
      _optIgnoreHints = True,
      _optIgnoreTags = True,
      _optNock = False
    }

nockOptions :: Options
nockOptions =
  Options
    { _optPrettyMode = MinimizeDelimiters,
      _optIgnoreHints = True,
      _optIgnoreTags = True,
      _optNock = True
    }

testOptions :: Options
testOptions =
  Options
    { _optPrettyMode = MinimizeDelimiters,
      _optIgnoreHints = False,
      _optIgnoreTags = True,
      _optNock = False
    }

traceOptions :: Options
traceOptions =
  Options
    { _optPrettyMode = MinimizeDelimiters,
      _optIgnoreHints = False,
      _optIgnoreTags = False,
      _optNock = False
    }

makeLenses ''Options

fromGenericOptions :: GenericOptions -> Options
fromGenericOptions GenericOptions {} = defaultOptions

instance CanonicalProjection GenericOptions Options where
  project = fromGenericOptions
