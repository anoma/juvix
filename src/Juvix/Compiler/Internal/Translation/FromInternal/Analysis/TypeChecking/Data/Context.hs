module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context,
    module Juvix.Compiler.Store.Internal.Data.TypeCheckingTables,
    module Juvix.Compiler.Internal.Data.InfoTable,
  )
where

import Juvix.Compiler.Internal.Data.InfoTable
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker (TerminationState)
import Juvix.Compiler.Store.Internal.Data.TypeCheckingTables
import Juvix.Prelude

data InternalTypedResult = InternalTypedResult
  { _resultInternal :: Internal.InternalResult,
    _resultModule :: Module,
    _resultInternalModule :: InternalModule,
    _resultTermination :: TerminationState,
    _resultTypeCheckingTables :: TypeCheckingTables
  }

newtype ImportContext = ImportContext
  { _importContextTables :: TypeCheckingTables
  }

makeLenses ''InternalTypedResult
makeLenses ''ImportContext

getInternalTypedResultComments :: InternalTypedResult -> Comments
getInternalTypedResultComments = Internal.getInternalResultComments . (^. resultInternal)
