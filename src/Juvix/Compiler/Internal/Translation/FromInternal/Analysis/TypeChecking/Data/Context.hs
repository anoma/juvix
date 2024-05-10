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
import Juvix.Compiler.Store.Internal.Data.CoercionInfo
import Juvix.Compiler.Store.Internal.Data.FunctionsTable
import Juvix.Compiler.Store.Internal.Data.InstanceInfo
import Juvix.Compiler.Store.Internal.Data.TypesTable
import Juvix.Prelude

data InternalTypedResult = InternalTypedResult
  { _resultInternal :: Internal.InternalResult,
    _resultModule :: Module,
    _resultInternalModule :: InternalModule,
    _resultTermination :: TerminationState,
    _resultIdenTypes :: TypesTable,
    _resultFunctions :: FunctionsTable
  }

data ImportContext = ImportContext
  { _importContextTypesTable :: TypesTable,
    _importContextFunctionsTable :: FunctionsTable,
    _importContextInstances :: InstanceTable,
    _importContextCoercions :: CoercionTable
  }

makeLenses ''InternalTypedResult
makeLenses ''ImportContext
