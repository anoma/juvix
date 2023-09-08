module Juvix.Compiler.Internal.Translation.FromInternal
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Reachability,
    arityChecking,
    typeChecking,
    typeCheckExpression,
    typeCheckExpressionType,
    arityCheckExpression,
    arityCheckImport,
    typeCheckImport,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Builtins.Effect
import Juvix.Compiler.Concrete.Data.Highlight.Input
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Pretty
import Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking qualified as ArityChecking
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Reachability
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking
import Juvix.Compiler.Pipeline.Artifacts
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Data.Effect.NameIdGen
import Juvix.Prelude hiding (fromEither)

arityChecking ::
  (Members '[Error JuvixError, NameIdGen] r) =>
  InternalResult ->
  Sem r ArityChecking.InternalArityResult
arityChecking res@InternalResult {..} =
  mapError (JuvixError @ArityChecking.ArityCheckerError) $ do
    r <-
      runReader table
        . evalCacheEmpty ArityChecking.checkModuleIndexNoCache
        $ mapM ArityChecking.checkModule _resultModules
    return
      ArityChecking.InternalArityResult
        { _resultInternalResult = res,
          _resultModules = r
        }
  where
    table :: InfoTable
    table = buildTable _resultModules

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

arityCheckImport ::
  (Members '[Error JuvixError, State Artifacts] r) =>
  Import ->
  Sem r Import
arityCheckImport i = do
  artiTable <- gets (^. artifactInternalTypedTable)
  let table = buildTable [i ^. importModule . moduleIxModule] <> artiTable
  mapError (JuvixError @ArityChecking.ArityCheckerError)
    . runReader table
    . runNameIdGenArtifacts
    . evalCacheEmpty ArityChecking.checkModuleIndexNoCache
    $ ArityChecking.checkImport i

typeCheckExpressionType ::
  forall r.
  (Members '[Error JuvixError, State Artifacts, Termination] r) =>
  Expression ->
  Sem r TypedExpression
typeCheckExpressionType exp = do
  table <- extendedTableReplArtifacts exp
  runTypesTableArtifacts
    . ignoreHighlightBuilder
    . runFunctionsTableArtifacts
    . runBuiltinsArtifacts
    . runNameIdGenArtifacts
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

typeCheckImport ::
  (Members '[Reader EntryPoint, Error JuvixError, State Artifacts, Termination] r) =>
  Import ->
  Sem r Import
typeCheckImport i = do
  artiTable <- gets (^. artifactInternalTypedTable)
  let table = buildTable [i ^. importModule . moduleIxModule] <> artiTable
  modify (set artifactInternalTypedTable table)
  mapError (JuvixError @TypeCheckerError)
    . runTypesTableArtifacts
    . runFunctionsTableArtifacts
    . ignoreHighlightBuilder
    . runBuiltinsArtifacts
    . runNameIdGenArtifacts
    . ignoreOutput @Example
    . runReader table
    . withEmptyVars
    -- TODO Store cache in Artifacts and use it here
    . evalCacheEmpty checkModuleNoCache
    $ checkImport i

typeChecking ::
  forall r.
  (Members '[HighlightBuilder, Error JuvixError, Builtins, NameIdGen] r) =>
  Sem (Termination ': r) ArityChecking.InternalArityResult ->
  Sem r InternalTypedResult
typeChecking a = do
  (termin, (res, (normalized, (idens, (funs, r))))) <- runTermination iniTerminationState $ do
    res <- a
    let table :: InfoTable
        table = buildTable (res ^. ArityChecking.resultModules)

        entryPoint :: EntryPoint
        entryPoint = res ^. ArityChecking.internalArityResultEntryPoint
    fmap (res,)
      . runOutputList
      . runReader entryPoint
      . runState (mempty :: TypesTable)
      . runState (mempty :: FunctionsTable)
      . runReader table
      . mapError (JuvixError @TypeCheckerError)
      . evalCacheEmpty checkModuleNoCache
      $ mapM checkModule (res ^. ArityChecking.resultModules)
  return
    InternalTypedResult
      { _resultInternalArityResult = res,
        _resultModules = r,
        _resultTermination = termin,
        _resultNormalized = HashMap.fromList [(e ^. exampleId, e ^. exampleExpression) | e <- normalized],
        _resultIdenTypes = idens,
        _resultFunctions = funs,
        _resultInfoTable = buildTable r
      }
