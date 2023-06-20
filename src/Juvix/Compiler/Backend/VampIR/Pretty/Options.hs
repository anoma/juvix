module Juvix.Compiler.Backend.VampIR.Pretty.Options where

import Juvix.Prelude

newtype Options = Options
  { _optIntegerBits :: Int
  }

makeLenses ''Options

defaultOptions :: Options
defaultOptions =
  Options
    { _optIntegerBits = 24
    }

traceOptions :: Options
traceOptions = defaultOptions

fromGenericOptions :: GenericOptions -> Options
fromGenericOptions _ = defaultOptions

instance CanonicalProjection GenericOptions Options where
  project = fromGenericOptions
