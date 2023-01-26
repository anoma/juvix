module Juvix.Compiler.Concrete.Data.ParsedInfoTable where

import Juvix.Compiler.Concrete.Data.ParsedItem
import Juvix.Compiler.Concrete.Language
import Juvix.Prelude

data InfoTable = InfoTable
  { _infoParsedItems :: [ParsedItem],
    _infoParsedComments :: Comments,
    _infoParsedModules :: HashMap TopModulePath (Module 'Parsed 'ModuleTop)
  }
  deriving stock (Eq, Show)

makeLenses ''InfoTable
