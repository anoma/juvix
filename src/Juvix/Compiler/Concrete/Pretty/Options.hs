module Juvix.Compiler.Concrete.Pretty.Options where

import Juvix.Prelude

data Options = Options
  { _optShowNameIds :: Bool,
    _optInlineImports :: Bool,
    _optApe :: Bool
  }

defaultOptions :: Options
defaultOptions =
  Options
    { _optShowNameIds = False,
      _optInlineImports = False,
      _optApe = False
    }

makeLenses ''Options

fromGenericOptions :: GenericOptions -> Options
fromGenericOptions GenericOptions {..} =
  set optShowNameIds _showNameIds $
    set
      optApe
      _genericApe
      defaultOptions
