module Juvix.Compiler.Core.Pretty.Options where

import Juvix.Prelude

data Options = Options
  { _optIndent :: Int,
    _optShowNameIds :: Bool,
    _optShowDeBruijnIndices :: Bool
  }

defaultOptions :: Options
defaultOptions =
  Options
    { _optIndent = 2,
      _optShowNameIds = False,
      _optShowDeBruijnIndices = False
    }

makeLenses ''Options

fromGenericOptions :: GenericOptions -> Options
fromGenericOptions GenericOptions {..} = set optShowNameIds _showNameIds defaultOptions
