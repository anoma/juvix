module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Data.Context where

import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context qualified as Scoped
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context qualified as Internal
import Juvix.Compiler.Pipeline.EntryPoint qualified as E
import Juvix.Prelude

data InternalArityResult = InternalArityResult
  { _resultInternalResult :: Internal.InternalResult,
    _resultModules :: NonEmpty Module
  }

makeLenses ''InternalArityResult

mainModule :: Lens' InternalArityResult Module
mainModule = resultModules . _head1

internalArityResultEntryPoint :: Lens' InternalArityResult E.EntryPoint
internalArityResultEntryPoint = resultInternalResult . Internal.internalResultEntryPoint

internalArityResultScoped :: Lens' InternalArityResult Scoped.ScoperResult
internalArityResultScoped = resultInternalResult . Internal.resultScoper
