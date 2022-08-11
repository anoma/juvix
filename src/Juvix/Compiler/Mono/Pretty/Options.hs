module Juvix.Compiler.Mono.Pretty.Options where

import Juvix.Prelude

newtype Options = Options
  { _optShowNameIds :: Bool
  }

defaultOptions :: Options
defaultOptions =
  Options
    { _optShowNameIds = False
    }

makeLenses ''Options
