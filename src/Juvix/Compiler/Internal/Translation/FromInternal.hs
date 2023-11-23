module Juvix.Compiler.Internal.Translation.FromInternal
  ( arityChecking,
    typeChecking,
    typeCheckExpression,
    typeCheckExpressionType,
    arityCheckExpression,
    arityCheckImport,
    typeCheckImport,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Concrete.Data.Highlight.Input
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking qualified as ArityChecking
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking
import Juvix.Compiler.Pipeline.Artifacts
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Store.Language
import Juvix.Data.Effect.NameIdGen
import Juvix.Prelude hiding (fromEither)

arityChecking ::
  (Members '[Error JuvixError, NameIdGen, Reader ModuleTable] r) =>
  InternalResult ->
  Sem r ArityChecking.InternalArityResult
arityChecking res@InternalResult {..} = do
  stab <- getInternalModuleTable <$> ask
  let table = computeCombinedInfoTable (insertInternalModule stab _resultInternalModule)
  mapError (JuvixError @ArityChecking.ArityCheckerError) $ do
    r <-
      runReader table $
        ArityChecking.checkModule _resultModule
    return
      ArityChecking.InternalArityResult
        { _resultInternal = res,
          _resultModule = r,
          _resultInternalModule = computeInternalModule mempty mempty r
        }

arityCheckExpression ::
  (Members '[Error JuvixError, State Artifacts] r) =>
  Expression ->
  Sem r Expression
arityCheckExpression exp = do
  table <- extendedTableReplArtifacts exp
  mapError (JuvixError @ArityChecking.ArityCheckerError)
    . runReader table
    . runNameIdGenArtifacts
    $ ArityChecking.inferReplExpression exp

arityCheckImport :: Import -> Sem r Import
arityCheckImport = return

typeCheckExpressionType ::
  forall r.
  (Members '[Error JuvixError, State Artifacts, Termination] r) =>
  Expression ->
  Sem r TypedExpression
typeCheckExpressionType exp = do
  table <- extendedTableReplArtifacts exp
  runTypesTableArtifacts
    . runFunctionsTableArtifacts
    . runBuiltinsArtifacts
    . runNameIdGenArtifacts
    . ignoreHighlightBuilder
    . runReader table
    . ignoreOutput @Example
    . withEmptyVars
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

typeChecking ::
  forall r.
  (Members '[Reader EntryPoint, Error JuvixError, NameIdGen, Reader ModuleTable] r) =>
  Sem (Termination ': r) ArityChecking.InternalArityResult ->
  Sem r InternalTypedResult
typeChecking a = do
  (termin, (res, (normalized, (idens, (funs, r))))) <- runTermination iniTerminationState $ do
    res <- a
    itab <- getInternalModuleTable <$> ask
    let md :: InternalModule
        md = res ^. ArityChecking.resultInternalModule
        table :: InfoTable
        table = computeCombinedInfoTable (insertInternalModule itab md)
    fmap (res,)
      . runOutputList
      . runState (computeTypesTable itab)
      . runState (computeFunctionsTable itab)
      . runReader table
      . mapError (JuvixError @TypeCheckerError)
      $ checkTable >> checkModule (res ^. ArityChecking.resultModule)
  return
    InternalTypedResult
      { _resultInternal = res,
        _resultModule = r,
        _resultInternalModule = computeInternalModule idens funs r,
        _resultTermination = termin,
        _resultNormalized = HashMap.fromList [(e ^. exampleId, e ^. exampleExpression) | e <- normalized],
        _resultIdenTypes = idens,
        _resultFunctions = funs
      }
