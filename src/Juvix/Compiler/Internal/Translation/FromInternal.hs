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
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Translation.FromAbstract.Data.Context
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking qualified as ArityChecking
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Reachability
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking
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
  (Members '[Error JuvixError, NameIdGen] r) =>
  InternalResult ->
  Expression ->
  Sem r Expression
arityCheckExpression InternalResult {..} exp =
  mapError (JuvixError @ArityChecking.ArityCheckerError) $
    runReader table (ArityChecking.inferReplExpression exp)
  where
    table :: InfoTable
    table = buildTableRepl exp _resultModules

typeCheckExpressionType ::
  (Members '[Error JuvixError, NameIdGen, Builtins] r) =>
  InternalTypedResult ->
  Expression ->
  Sem r TypedExpression
typeCheckExpressionType InternalTypedResult {..} exp =
  mapError (JuvixError @TypeCheckerError)
    $ do
      runReader _resultFunctions
      . evalState _resultIdenTypes
      . runReader table
      . ignoreOutput @Example
      . withEmptyVars
      . runInferenceDef
    $ inferExpression' Nothing exp
  where
    table :: InfoTable
    table = buildTableRepl exp _resultModules

typeCheckExpression ::
  (Members '[Error JuvixError, NameIdGen, Builtins] r) =>
  InternalTypedResult ->
  Expression ->
  Sem r Expression
typeCheckExpression res exp = fmap (^. typedExpression) (typeCheckExpressionType res exp)

inferExpressionType ::
  (Members '[Error JuvixError, NameIdGen, Builtins] r) =>
  InternalTypedResult ->
  Expression ->
  Sem r Expression
inferExpressionType res exp = fmap (^. typedType) (typeCheckExpressionType res exp)

typeChecking ::
  (Members '[Error JuvixError, NameIdGen, Builtins] r) =>
  ArityChecking.InternalArityResult ->
  Sem r InternalTypedResult
typeChecking res@ArityChecking.InternalArityResult {..} =
  mapError (JuvixError @TypeCheckerError) $ do
    (normalized, (idens, (funs, r))) <-
      runOutputList
        . runState (mempty :: TypesTable)
        . runReader entryPoint
        . runState (mempty :: FunctionsTable)
        . runReader table
        $ mapM checkModule _resultModules
    return
      InternalTypedResult
        { _resultInternalArityResult = res,
          _resultModules = r,
          _resultNormalized = HashMap.fromList [(e ^. exampleId, e ^. exampleExpression) | e <- normalized],
          _resultIdenTypes = idens,
          _resultFunctions = funs
        }
  where
    table :: InfoTable
    table = buildTable _resultModules

    entryPoint :: EntryPoint
    entryPoint = res ^. ArityChecking.internalArityResultEntryPoint
