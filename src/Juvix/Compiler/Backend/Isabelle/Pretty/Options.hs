module Juvix.Compiler.Backend.Isabelle.Pretty.Options where

import Juvix.Prelude

data Options = Options
  { _optComments :: [Comment]
  }

makeLenses ''Options

defaultOptions :: [Comment] -> Options
defaultOptions = Options

traceOptions :: Options
traceOptions = defaultOptions []

fromGenericOptions :: GenericOptions -> Options
fromGenericOptions _ = defaultOptions []

instance CanonicalProjection GenericOptions Options where
  project = fromGenericOptions
