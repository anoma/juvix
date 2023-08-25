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
import Juvix.Compiler.Internal.Translation.FromConcrete qualified as Internal
import Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking qualified as ArityChecking
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Reachability
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker (NoLexOrder (NoLexOrder), TerminationError (ErrNoLexOrder))
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
  (Members '[Error JuvixError, State Artifacts] r) =>
  Expression ->
  Sem r TypedExpression
typeCheckExpressionType exp = do
  table <- extendedTableReplArtifacts exp
  nonTermin <- gets (^. artifactNonTerminating)
  mapError (JuvixError @TypeCheckerError)
    . runTypesTableArtifacts
    . ignoreHighlightBuilder
    . runFunctionsTableArtifacts
    . runBuiltinsArtifacts
    . runNameIdGenArtifacts
    . runReader table
    . ignoreOutput @Example
    . withEmptyVars
    . runInferenceDef
    . runReader nonTermin
    $ inferExpression' Nothing exp >>= traverseOf typedType strongNormalize

typeCheckExpression ::
  (Members '[Error JuvixError, State Artifacts] r) =>
  Expression ->
  Sem r Expression
typeCheckExpression exp = (^. typedExpression) <$> typeCheckExpressionType exp

typeCheckImport ::
  (Members '[Reader EntryPoint, Error JuvixError, State Artifacts] r) =>
  Import ->
  Sem r Import
typeCheckImport i = do
  artiTable <- gets (^. artifactInternalTypedTable)
  nonTermin <- gets (^. artifactNonTerminating)
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
    . runReader nonTermin
    -- TODO Store cache in Artifacts and use it here
    . evalCacheEmpty checkModuleNoCache
    $ checkImport i

typeChecking ::
  forall r.
  (Members '[HighlightBuilder, Error JuvixError, Builtins, NameIdGen] r) =>
  ArityChecking.InternalArityResult ->
  Sem r InternalTypedResult
typeChecking res@ArityChecking.InternalArityResult {..} = do
  mapError (JuvixError @TypeCheckerError) $ do
    (normalized, (idens, (funs, r))) <-
      runOutputList
        . runReader entryPoint
        . runState (mempty :: TypesTable)
        . runState (mempty :: FunctionsTable)
        . runReader table
        . runReader nonTermin
        . evalCacheEmpty checkModuleNoCache
        $ mapM checkModule _resultModules
    mapError (JuvixError @TerminationError) checkTerminating
    return
      InternalTypedResult
        { _resultInternalArityResult = res,
          _resultModules = r,
          _resultNormalized = HashMap.fromList [(e ^. exampleId, e ^. exampleExpression) | e <- normalized],
          _resultIdenTypes = idens,
          _resultFunctions = funs,
          _resultInfoTable = buildTable r
        }
  where
    nonTermin :: NonTerminating
    nonTermin = _resultInternalResult ^. Internal.resultNonTerminating

    checkTerminating :: (Members '[Error TerminationError] s) => Sem s ()
    checkTerminating = case nonTermin ^? nonTerminating . to toList . _head of
      Nothing -> return ()
      Just x -> throw (ErrNoLexOrder (NoLexOrder x))

    table :: InfoTable
    table = buildTable _resultModules

    entryPoint :: EntryPoint
    entryPoint = res ^. ArityChecking.internalArityResultEntryPoint
