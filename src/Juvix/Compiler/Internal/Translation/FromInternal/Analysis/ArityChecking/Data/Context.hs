module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Data.Context where

import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context
import Juvix.Prelude

data InternalArityResult = InternalArityResult
  { _resultInternal :: InternalResult,
    _resultModule :: Module,
    _resultStoredModule :: StoredModule
  }

makeLenses ''InternalArityResult
