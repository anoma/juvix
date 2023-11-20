module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context,
    module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.FunctionsTable,
    module Juvix.Compiler.Internal.Data.InfoTable,
  )
where

import Juvix.Compiler.Internal.Data.InfoTable
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker (TerminationState)
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.FunctionsTable
import Juvix.Prelude

type TypesTable = HashMap NameId Expression

type NormalizedTable = HashMap NameId Expression

data InternalTypedResult = InternalTypedResult
  { _resultModule :: Module,
    _resultInternalModule :: InternalModule,
    _resultTermination :: TerminationState,
    _resultNormalized :: NormalizedTable,
    _resultIdenTypes :: TypesTable,
    _resultFunctions :: FunctionsTable
  }

makeLenses ''InternalTypedResult
