module Juvix.Compiler.Backend.Rust.Pretty.Options where

import Juvix.Compiler.Backend.Rust.Data.Backend
import Juvix.Prelude

data Options = Options
  { _optBackend :: Backend
  }

makeLenses ''Options

defaultOptions :: Options
defaultOptions =
  Options
    { _optBackend = BackendRust
    }

traceOptions :: Options
traceOptions = defaultOptions

fromGenericOptions :: GenericOptions -> Options
fromGenericOptions _ = defaultOptions

instance CanonicalProjection GenericOptions Options where
  project = fromGenericOptions
