module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Data.Context where

import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Store.Internal.Language
import Juvix.Prelude

data InternalArityResult = InternalArityResult
  { _resultModule :: Module,
    _resultStoredModule :: StoredModule
  }

makeLenses ''InternalArityResult
