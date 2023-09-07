module Juvix.Compiler.Backend.VampIR.Pretty.Options where

import Juvix.Prelude

data Options = Options
  { _optIntegerBits :: Int,
    _optUnsafe :: Bool
  }

makeLenses ''Options

defaultOptions :: Options
defaultOptions =
  Options
    { _optIntegerBits = 24,
      _optUnsafe = False
    }

traceOptions :: Options
traceOptions = defaultOptions

fromGenericOptions :: GenericOptions -> Options
fromGenericOptions _ = defaultOptions

instance CanonicalProjection GenericOptions Options where
  project = fromGenericOptions
