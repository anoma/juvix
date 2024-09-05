module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context,
    module Juvix.Compiler.Store.Internal.Data.FunctionsTable,
    module Juvix.Compiler.Store.Internal.Data.TypesTable,
    module Juvix.Compiler.Internal.Data.InfoTable,
    module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Data.Base,
  )
where

import Juvix.Compiler.Internal.Data.InfoTable
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker (TerminationState)
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Data.Base
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
    -- | Used only in `juvix dev instance-termination calls`
    _resultInstanceCallMaps :: InstanceCallMaps,
    _resultIdenTypes :: TypesTable,
    _resultFunctions :: FunctionsTable,
    _resultInstances :: InstanceTable,
    _resultCoercions :: CoercionTable
  }

data InstanceCallMap = InstanceCallMap
  { _instanceCallMapBlock :: MutualBlock,
    _instanceCallMap :: CallMap' InstanceParam
  }

newtype InstanceCallMaps = InstanceCallMaps
  { _instanceCallMaps :: [InstanceCallMap]
  }
  deriving newtype (Semigroup, Monoid)

data ImportContext = ImportContext
  { _importContextTypesTable :: TypesTable,
    _importContextFunctionsTable :: FunctionsTable,
    _importContextInstances :: InstanceTable,
    _importContextCoercions :: CoercionTable
  }

makeLenses ''InternalTypedResult
makeLenses ''ImportContext
makeLenses ''InstanceCallMaps
makeLenses ''InstanceCallMap

getInternalTypedResultComments :: InternalTypedResult -> Comments
getInternalTypedResultComments = Internal.getInternalResultComments . (^. resultInternal)
