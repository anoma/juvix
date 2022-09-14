module Juvix.Compiler.Asm.Pretty.Options where

import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Core.Pretty.Options qualified as Core
import Juvix.Prelude

data Options = Options
  { _optIndent :: Int,
    _optInfoTable :: InfoTable
  }

makeLenses ''Options

defaultOptions :: InfoTable -> Options
defaultOptions tab =
  Options
    { _optIndent = 2,
      _optInfoTable = tab
    }

toCoreOptions :: Options -> Core.Options
toCoreOptions Options {..} = Core.defaultOptions {Core._optIndent = _optIndent}
