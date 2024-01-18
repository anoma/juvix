module Juvix.Compiler.Asm.Pretty.Options where

import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Core.Pretty.Options qualified as Core
import Juvix.Prelude

newtype Options = Options
  { _optInfoTable :: InfoTable
  }

makeLenses ''Options

defaultOptions :: InfoTable -> Options
defaultOptions tab =
  Options
    { _optInfoTable = tab
    }

toCoreOptions :: Options -> Core.Options
toCoreOptions Options {} = Core.defaultOptions
