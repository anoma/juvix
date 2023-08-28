module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context,
    module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.FunctionsTable,
    module Juvix.Compiler.Internal.Data.InfoTable,
  )
where

import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context qualified as Scoped
import Juvix.Compiler.Internal.Data.InfoTable
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Data.Context (InternalArityResult)
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Data.Context qualified as Arity
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker (TerminationState)
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.FunctionsTable
import Juvix.Compiler.Pipeline.EntryPoint qualified as E
import Juvix.Prelude

type TypesTable = HashMap NameId Expression

type NormalizedTable = HashMap NameId Expression

data InternalTypedResult = InternalTypedResult
  { _resultInternalArityResult :: InternalArityResult,
    _resultModules :: NonEmpty Module,
    _resultTermination :: TerminationState,
    _resultNormalized :: NormalizedTable,
    _resultIdenTypes :: TypesTable,
    _resultFunctions :: FunctionsTable,
    _resultInfoTable :: InfoTable
  }

makeLenses ''InternalTypedResult

mainModule :: Lens' InternalTypedResult Module
mainModule = resultModules . _head1

internalTypedResultEntryPoint :: Lens' InternalTypedResult E.EntryPoint
internalTypedResultEntryPoint = resultInternalArityResult . Arity.internalArityResultEntryPoint

internalTypedResultScoped :: Lens' InternalTypedResult Scoped.ScoperResult
internalTypedResultScoped = resultInternalArityResult . Arity.internalArityResultScoped
