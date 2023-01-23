module Juvix.Compiler.Backend.Geb.Pretty.Options where

import Juvix.Prelude

newtype Options = Options
  { _optIndent :: Int
  }

makeLenses ''Options

defaultOptions :: Options
defaultOptions =
  Options
    { _optIndent = 2
    }

traceOptions :: Options
traceOptions =
  Options
    { _optIndent = 2
    }

fromGenericOptions :: GenericOptions -> Options
fromGenericOptions _ = defaultOptions

instance CanonicalProjection GenericOptions Options where
  project = fromGenericOptions
