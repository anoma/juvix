module Juvix.Compiler.Internal.Translation.FromInternal
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Reachability,
    typeChecking,
    typeCheckingNew,
    typeCheckExpression,
    typeCheckExpressionType,
    typeCheckImport,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Builtins.Effect
import Juvix.Compiler.Concrete.Data.Highlight.Input
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Pretty
import Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Reachability
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking
import Juvix.Compiler.Pipeline.Artifacts
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Data.Effect.NameIdGen
import Juvix.Prelude hiding (fromEither)

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
    . withEmptyLocalVars
    -- TODO Store cache in Artifacts and use it here
    . evalCacheEmpty checkModuleNoCache
    $ checkTable >> checkImport i

typeChecking ::
  forall r.
  (Members '[HighlightBuilder, Error JuvixError, Builtins, NameIdGen] r) =>
  Sem (Termination ': r) Internal.InternalResult ->
  Sem r InternalTypedResult
typeChecking a = do
  (termin, (res, table, (normalized, (idens, (funs, r))))) <- runTermination iniTerminationState $ do
    res <- a
    let table :: InfoTable
        table = buildTable (res ^. Internal.resultModules)

        entryPoint :: EntryPoint
        entryPoint = res ^. Internal.internalResultEntryPoint
    fmap (res,table,)
      . runOutputList
      . runReader entryPoint
      . runState (mempty :: TypesTable)
      . runState (mempty :: FunctionsTable)
      . runReader table
      . mapError (JuvixError @TypeCheckerError)
      . evalCacheEmpty checkModuleNoCache
      $ checkTable >> mapM checkModule (res ^. Internal.resultModules)
  return
    InternalTypedResult
      { _resultInternalResult = res,
        _resultModules = r,
        _resultTermination = termin,
        _resultNormalized = HashMap.fromList [(e ^. exampleId, e ^. exampleExpression) | e <- normalized],
        _resultIdenTypes = idens,
        _resultFunctions = funs,
        _resultInfoTable = table
      }

typeCheckingNew ::
  forall r.
  (Members '[HighlightBuilder, Error JuvixError, Builtins, NameIdGen] r) =>
  Sem (Termination ': r) InternalResult ->
  Sem r InternalTypedResult
typeCheckingNew a = do
  (termin, (res, table, (normalized, (idens, (funs, r))))) <- runTermination iniTerminationState $ do
    res :: InternalResult <- a
    let table :: InfoTable
        table = buildTable (res ^. Internal.resultModules)

        entryPoint :: EntryPoint
        entryPoint = res ^. Internal.internalResultEntryPoint
    fmap (res,table,)
      . runOutputList
      . runReader entryPoint
      . runState (mempty :: TypesTable)
      . runState (mempty :: FunctionsTable)
      . runReader table
      . mapError (JuvixError @TypeCheckerError)
      . evalCacheEmpty checkModuleNoCache
      $ checkTable >> mapM checkModule (res ^. Internal.resultModules)
  return
    InternalTypedResult
      { _resultInternalResult = res,
        _resultModules = r,
        _resultTermination = termin,
        _resultNormalized = HashMap.fromList [(e ^. exampleId, e ^. exampleExpression) | e <- normalized],
        _resultIdenTypes = idens,
        _resultFunctions = funs,
        _resultInfoTable = table
      }
