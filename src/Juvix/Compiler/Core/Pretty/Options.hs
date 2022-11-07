module Juvix.Compiler.Core.Pretty.Options where

import Juvix.Prelude

data Options = Options
  { _optIndent :: Int,
    _optShowNameIds :: Bool,
    _optShowDeBruijnIndices :: Bool
  }

makeLenses ''Options

defaultOptions :: Options
defaultOptions =
  Options
    { _optIndent = 2,
      _optShowNameIds = False,
      _optShowDeBruijnIndices = False
    }

traceOptions :: Options
traceOptions =
  Options
    { _optIndent = 2,
      _optShowNameIds = False,
      _optShowDeBruijnIndices = True
    }

fromGenericOptions :: GenericOptions -> Options
fromGenericOptions GenericOptions {..} = set optShowNameIds _showNameIds defaultOptions

instance CanonicalProjection GenericOptions Options where
  project = fromGenericOptions
