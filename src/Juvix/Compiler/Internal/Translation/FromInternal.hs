module Juvix.Compiler.Internal.Translation.FromInternal
  ( typeCheckingNew,
    typeCheckExpression,
    typeCheckExpressionType,
    typeCheckImport,
  )
where

import Juvix.Compiler.Concrete.Data.Highlight.Input
import Juvix.Compiler.Internal.Data.InfoTable
import Juvix.Compiler.Internal.Data.LocalVars
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking
import Juvix.Compiler.Pipeline.Artifacts
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Store.Extra
import Juvix.Compiler.Store.Language
import Juvix.Data.Effect.NameIdGen
import Juvix.Prelude hiding (fromEither)

typeCheckExpressionType ::
  forall r.
  (Members '[Error JuvixError, State Artifacts, Termination] r) =>
  Expression ->
  Sem r TypedExpression
typeCheckExpressionType exp = do
  -- TODO: refactor: modules outside of REPL should not refer to Artifacts
  table <- extendedTableReplArtifacts exp
  runResultBuilderArtifacts
    . runBuiltinsArtifacts
    . runNameIdGenArtifacts
    . ignoreHighlightBuilder
    . runReader table
    . withEmptyLocalVars
    . withEmptyInsertedArgsStack
    . mapError (JuvixError @TypeCheckerError)
    . runInferenceDef
    $ inferExpression Nothing exp
      >>= traverseOf typedType strongNormalize

typeCheckExpression ::
  (Members '[Error JuvixError, State Artifacts, Termination] r) =>
  Expression ->
  Sem r Expression
typeCheckExpression exp = (^. typedExpression) <$> typeCheckExpressionType exp

typeCheckImport :: Import -> Sem r Import
typeCheckImport = return

typeCheckingNew ::
  forall r.
  (Members '[HighlightBuilder, Reader EntryPoint, Error JuvixError, NameIdGen, Reader ModuleTable] r) =>
  Sem (Termination ': r) InternalResult ->
  Sem r InternalTypedResult
typeCheckingNew a = do
  (termin, (res, (bst, checkedModule))) <- runTermination iniTerminationState $ do
    res <- a
    itab <- getInternalModuleTable <$> ask
    let table :: InfoTable
        table = computeCombinedInfoTable itab <> computeInternalModuleInfoTable (res ^. Internal.resultModule)
        importCtx =
          ImportContext
            { _importContextTypesTable = computeTypesTable itab,
              _importContextFunctionsTable = computeFunctionsTable itab,
              _importContextInstances = computeInstanceTable itab,
              _importContextCoercions = computeCoercionTable itab
            }
    fmap (res,)
      . runReader table
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
