module Juvix.Compiler.Backend.VampIR.Pretty.Options where

import Juvix.Prelude

-- no fields for now, but make it easier to add options in the future I don't
-- remove this datatype entirely
data Options = Options

makeLenses ''Options

defaultOptions :: Options
defaultOptions = Options

traceOptions :: Options
traceOptions = defaultOptions

fromGenericOptions :: GenericOptions -> Options
fromGenericOptions _ = defaultOptions

instance CanonicalProjection GenericOptions Options where
  project = fromGenericOptions
