module Juvix.Compiler.Internal.Pretty.Options where

import Juvix.Prelude

newtype Options = Options
  { _optShowNameIds :: Bool
  }

defaultOptions :: Options
defaultOptions =
  Options
    { _optShowNameIds = False
    }

traceOptions :: Options
traceOptions =
  Options
    { _optShowNameIds = True
    }

makeLenses ''Options

fromGenericOptions :: GenericOptions -> Options
fromGenericOptions GenericOptions {..} = Options {_optShowNameIds = _showNameIds}

instance CanonicalProjection GenericOptions Options where
  project = fromGenericOptions
