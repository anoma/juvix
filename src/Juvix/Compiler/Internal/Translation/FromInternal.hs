module Juvix.Compiler.Internal.Translation.FromInternal
  ( typeCheckingNew,
  )
where

import Juvix.Compiler.Concrete.Data.Highlight.Input
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context
import Juvix.Compiler.Internal.Data.InfoTable as Internal
import Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Store.Extra
import Juvix.Compiler.Store.Language
import Juvix.Compiler.Store.Scoped.Data.InfoTable qualified as Scoped
import Juvix.Compiler.Store.Scoped.Language
import Juvix.Compiler.Store.Scoped.Language qualified as Scoped
import Juvix.Data.Effect.NameIdGen
import Juvix.Prelude hiding (fromEither)

typeCheckingNew ::
  forall r.
  (Members '[HighlightBuilder, Reader EntryPoint, Error JuvixError, NameIdGen, Reader ModuleTable] r) =>
  Sem (Termination ': r) InternalResult ->
  Sem r InternalTypedResult
typeCheckingNew a = do
  (termin, (res, (bst, checkedModule))) <- runTermination iniTerminationState $ do
    res :: InternalResult <- a
    itab :: InternalModuleTable <- getInternalModuleTable <$> ask
    stab :: ScopedModuleTable <- getScopedModuleTable <$> ask
    let table :: InfoTable
        table = Internal.computeCombinedInfoTable itab <> computeInternalModuleInfoTable (res ^. Internal.resultModule)
        stable :: Scoped.InfoTable
        stable = Scoped.computeCombinedInfoTable stab <> (res ^. Internal.resultScoper . resultScopedModule . scopedModuleInfoTable)
        importCtx =
          ImportContext
            { _importContextTypesTable = computeTypesTable itab,
              _importContextFunctionsTable = computeFunctionsTable itab,
              _importContextInstances = computeInstanceTable itab,
              _importContextCoercions = computeCoercionTable itab
            }
    fmap (res,)
      . runReader table
      . runReader (stable ^. Scoped.infoBuiltins)
      . runResultBuilder importCtx
      . mapError (JuvixError @TypeCheckerError)
      $ checkTopModule (res ^. Internal.resultModule)
  let md =
        computeInternalModule
          (bst ^. resultBuilderStateInstanceTable)
          (bst ^. resultBuilderStateCoercionTable)
          (bst ^. resultBuilderStateTypesTable)
          (bst ^. resultBuilderStateFunctionsTable)
          checkedModule
  return
    InternalTypedResult
      { _resultInternal = res,
        _resultModule = checkedModule,
        _resultInternalModule = md,
        _resultTermination = termin,
        _resultIdenTypes = bst ^. resultBuilderStateCombinedTypesTable,
        _resultFunctions = bst ^. resultBuilderStateCombinedFunctionsTable,
        _resultInstances = bst ^. resultBuilderStateCombinedInstanceTable,
        _resultCoercions = bst ^. resultBuilderStateCombinedCoercionTable
      }
