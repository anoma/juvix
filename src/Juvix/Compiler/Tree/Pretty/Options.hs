module Juvix.Compiler.Tree.Pretty.Options where

import Juvix.Compiler.Core.Pretty.Options qualified as Core
import Juvix.Compiler.Tree.Data.InfoTable.Base
import Juvix.Compiler.Tree.Language.Base

data Options = Options
  { _optSymbolNames :: HashMap Symbol Text,
    _optTagNames :: HashMap Tag Text
  }

makeLenses ''Options

defaultOptions :: InfoTable' t e -> Options
defaultOptions tab =
  Options
    { _optSymbolNames =
        fmap (^. functionName) (tab ^. infoFunctions)
          <> fmap (^. inductiveName) (tab ^. infoInductives),
      _optTagNames =
        fmap (^. constructorName) (tab ^. infoConstrs)
    }

toCoreOptions :: Options -> Core.Options
toCoreOptions Options {} = Core.defaultOptions
