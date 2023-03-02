module Juvix.Compiler.Core.Pretty.Options where

import Juvix.Prelude

data Options = Options
  { _optShowIdentIds :: Bool,
    _optShowDeBruijnIndices :: Bool
  }

makeLenses ''Options

defaultOptions :: Options
defaultOptions =
  Options
    { _optShowIdentIds = False,
      _optShowDeBruijnIndices = False
    }

traceOptions :: Options
traceOptions =
  Options
    { _optShowIdentIds = False,
      _optShowDeBruijnIndices = True
    }

fromGenericOptions :: GenericOptions -> Options
fromGenericOptions _ = defaultOptions

instance CanonicalProjection GenericOptions Options where
  project _ = defaultOptions
