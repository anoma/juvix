module Juvix.Compiler.Nockma.Pretty.Options where

import Juvix.Prelude

data PrettyMode
  = MinimizeDelimiters
  | AllDelimiters
  deriving stock (Data)

defaultOptions :: Options
defaultOptions =
  Options
    { _optPrettyMode = MinimizeDelimiters
    }

newtype Options = Options
  { _optPrettyMode :: PrettyMode
  }

traceOptions :: Options
traceOptions =
  Options
    { _optPrettyMode = AllDelimiters
    }

makeLenses ''Options

fromGenericOptions :: GenericOptions -> Options
fromGenericOptions GenericOptions {} = defaultOptions

instance CanonicalProjection GenericOptions Options where
  project = fromGenericOptions
