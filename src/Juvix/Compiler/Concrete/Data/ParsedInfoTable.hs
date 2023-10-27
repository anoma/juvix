module Juvix.Compiler.Concrete.Data.ParsedInfoTable where

import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Store.Scoped.Language
import Juvix.Prelude

data InfoTable = InfoTable
  { _infoParsedComments :: Comments,
    _infoParsedModules :: HashMap TopModulePath ScopedModule
  }

makeLenses ''InfoTable
