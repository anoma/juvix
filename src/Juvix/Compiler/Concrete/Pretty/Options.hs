module Juvix.Compiler.Concrete.Pretty.Options
  ( module Juvix.Compiler.Concrete.Pretty.Options,
    module Juvix.Compiler.Concrete.Data.Rename,
  )
where

import Juvix.Compiler.Concrete.Data.Rename
import Juvix.Prelude

data Options = Options
  { _optShowNameIds :: Bool,
    _optInJudocBlock :: Bool,
    _optRenames :: [Rename],
    _optPrintPragmas :: Bool
  }

defaultOptions :: Options
defaultOptions =
  Options
    { _optShowNameIds = False,
      _optInJudocBlock = False,
      _optRenames = [],
      _optPrintPragmas = True
    }

traceOptions :: Options
traceOptions =
  Options
    { _optShowNameIds = True,
      _optInJudocBlock = False,
      _optRenames = [],
      _optPrintPragmas = True
    }

makeLenses ''Options

fromGenericOptions :: GenericOptions -> Options
fromGenericOptions GenericOptions {..} =
  set optShowNameIds _showNameIds defaultOptions

inJudocBlock :: (Members '[Reader Options] r) => Sem r a -> Sem r a
inJudocBlock = local (set optInJudocBlock True)

instance CanonicalProjection GenericOptions Options where
  project = fromGenericOptions
