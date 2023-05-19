module Juvix.Compiler.Concrete.Pretty.Options where

import Juvix.Prelude

data Options = Options
  { _optShowNameIds :: Bool,
    _optNoApe :: Bool,
    _optInJudocBlock :: Bool
  }

defaultOptions :: Options
defaultOptions =
  Options
    { _optShowNameIds = False,
      _optNoApe = False,
      _optInJudocBlock = False
    }

makeLenses ''Options

fromGenericOptions :: GenericOptions -> Options
fromGenericOptions GenericOptions {..} =
  set optShowNameIds _showNameIds $
    set
      optNoApe
      _genericNoApe
      defaultOptions

inJudocBlock :: Members '[Reader Options] r => Sem r a -> Sem r a
inJudocBlock = local (set optInJudocBlock True)
