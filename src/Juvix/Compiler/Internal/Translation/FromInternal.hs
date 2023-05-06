module Juvix.Compiler.Internal.Translation.FromInternal
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Reachability,
    arityChecking,
    typeChecking,
    typeCheckExpression,
    arityCheckExpression,
    inferExpressionType,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Builtins.Effect
import Juvix.Compiler.Concrete.Data.Highlight.Input
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Translation.FromAbstract.Data.Context
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking qualified as ArityChecking
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Reachability
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
    r <- runReader table (mapM ArityChecking.checkModule _resultModules)
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
    $ runReader table
      . runNameIdGenArtifacts
    $ ArityChecking.inferReplExpression exp

typeCheckExpressionType ::
  forall r.
  (Members '[Error JuvixError, State Artifacts] r) =>
  Expression ->
  Sem r TypedExpression
typeCheckExpressionType exp = do
  table <- extendedTableReplArtifacts exp
  mapError (JuvixError @TypeCheckerError)
    $ runTypesTableArtifacts
      . ignoreHighlightBuilder
      . runFunctionsTableArtifacts
      . runBuiltinsArtifacts
      . runNameIdGenArtifacts
      . runReader table
      . ignoreOutput @Example
      . withEmptyVars
      . runInferenceDef
    $ inferExpression' Nothing exp >>= traverseOf typedType strongNormalize

typeCheckExpression ::
  (Members '[Error JuvixError, State Artifacts] r) =>
  Expression ->
  Sem r Expression
typeCheckExpression exp = (^. typedExpression) <$> typeCheckExpressionType exp

inferExpressionType ::
  (Members '[Error JuvixError, State Artifacts] r) =>
  Expression ->
  Sem r Expression
inferExpressionType exp = (^. typedType) <$> typeCheckExpressionType exp

typeChecking ::
  Members '[HighlightBuilder, Error JuvixError, Builtins, NameIdGen] r =>
  ArityChecking.InternalArityResult ->
  Sem r InternalTypedResult
typeChecking res@ArityChecking.InternalArityResult {..} =
  mapError (JuvixError @TypeCheckerError) $ do
    (normalized, (idens, (funs, r))) <-
      runOutputList
        . runReader entryPoint
        . runState (mempty :: TypesTable)
        . runState (mempty :: FunctionsTable)
        . runReader table
        $ mapM checkModule _resultModules
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
    table :: InfoTable
    table = buildTable _resultModules

    entryPoint :: EntryPoint
    entryPoint = res ^. ArityChecking.internalArityResultEntryPoint
