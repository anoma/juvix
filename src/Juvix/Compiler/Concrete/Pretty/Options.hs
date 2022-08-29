module Juvix.Compiler.Concrete.Pretty.Options where

import Juvix.Prelude

data Options = Options
  { _optShowNameIds :: Bool,
    _optInlineImports :: Bool
  }

defaultOptions :: Options
defaultOptions =
  Options
    { _optShowNameIds = False,
      _optInlineImports = False
    }

makeLenses ''Options

fromGenericOptions :: GenericOptions -> Options
fromGenericOptions GenericOptions {..} = set optShowNameIds _optShowNameIds defaultOptions
