module Juvix.Compiler.Verification.Core.Pretty.Options where

import Juvix.Prelude

data Options = Options

defaultOptions :: Options
defaultOptions = Options

traceOptions :: Options
traceOptions = Options

fromGenericOptions :: GenericOptions -> Options
fromGenericOptions _ = defaultOptions

instance CanonicalProjection GenericOptions Options where
  project _ = defaultOptions
