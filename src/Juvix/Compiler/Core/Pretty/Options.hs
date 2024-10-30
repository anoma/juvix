module Juvix.Compiler.Core.Pretty.Options where

import Juvix.Prelude

data Options = Options
  { _optShowIdentIds :: Bool,
    _optShowDeBruijnIndices :: Bool,
    _optShowArgsNum :: Bool,
    _optPrettyPatterns :: Bool
  }

makeLenses ''Options

defaultOptions :: Options
defaultOptions =
  Options
    { _optShowIdentIds = False,
      _optShowDeBruijnIndices = False,
      _optShowArgsNum = False,
      _optPrettyPatterns = False
    }

traceOptions :: Options
traceOptions =
  Options
    { _optShowIdentIds = True,
      _optShowDeBruijnIndices = True,
      _optShowArgsNum = True,
      _optPrettyPatterns = False
    }

fromGenericOptions :: GenericOptions -> Options
fromGenericOptions _ = defaultOptions

instance CanonicalProjection GenericOptions Options where
  project _ = defaultOptions
