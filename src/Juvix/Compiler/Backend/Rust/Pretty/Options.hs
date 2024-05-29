module Juvix.Compiler.Backend.Rust.Pretty.Options where

import Juvix.Prelude

data Options = Options
  { _optRiscZero :: Bool
  }

makeLenses ''Options

defaultOptions :: Options
defaultOptions =
  Options
    { _optRiscZero = False
    }

traceOptions :: Options
traceOptions = defaultOptions

fromGenericOptions :: GenericOptions -> Options
fromGenericOptions _ = defaultOptions

instance CanonicalProjection GenericOptions Options where
  project = fromGenericOptions
