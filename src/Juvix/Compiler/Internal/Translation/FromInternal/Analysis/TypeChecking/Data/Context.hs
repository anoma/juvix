module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context,
    module Juvix.Compiler.Store.Internal.Data.FunctionsTable,
    module Juvix.Compiler.Store.Internal.Data.TypesTable,
    module Juvix.Compiler.Internal.Data.InfoTable,
  )
where

import Juvix.Compiler.Internal.Data.InfoTable
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker (TerminationState)
import Juvix.Compiler.Store.Internal.Data.FunctionsTable
import Juvix.Compiler.Store.Internal.Data.TypesTable
import Juvix.Prelude

type NormalizedTable = HashMap NameId Expression

data InternalTypedResult = InternalTypedResult
  { _resultInternal :: Internal.InternalResult,
    _resultModule :: Module,
    _resultInternalModule :: InternalModule,
    _resultTermination :: TerminationState,
    _resultNormalized :: NormalizedTable,
    _resultIdenTypes :: TypesTable,
    _resultFunctions :: FunctionsTable
  }

makeLenses ''TypesTable
makeLenses ''InternalTypedResult
