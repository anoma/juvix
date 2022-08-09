module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context,
    module Juvix.Compiler.Internal.Data.InfoTable,
  )
where

import Juvix.Compiler.Internal.Data.InfoTable
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Data.Context (InternalArityResult)
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Data.Context qualified as Arity
import Juvix.Compiler.Pipeline.EntryPoint qualified as E
import Juvix.Prelude

type TypesTable = HashMap Name Expression

type NormalizedTable = HashMap NameId Expression

data InternalTypedResult = InternalTypedResult
  { _resultInternalArityResult :: InternalArityResult,
    _resultModules :: NonEmpty Module,
    _resultNormalized :: NormalizedTable,
    _resultIdenTypes :: TypesTable
  }

makeLenses ''InternalTypedResult

mainModule :: Lens' InternalTypedResult Module
mainModule = resultModules . _head

internalTypedResultEntryPoint :: Lens' InternalTypedResult E.EntryPoint
internalTypedResultEntryPoint = resultInternalArityResult . Arity.internalArityResultEntryPoint
