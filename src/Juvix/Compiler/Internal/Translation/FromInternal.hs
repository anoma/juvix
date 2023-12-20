module Juvix.Compiler.Internal.Translation.FromInternal
  ( typeCheckingNew,
    typeCheckExpression,
    typeCheckExpressionType,
    typeCheckImport,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Concrete.Data.Highlight.Input
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
  table <- extendedTableReplArtifacts exp
  runTypesTableArtifacts
    . runFunctionsTableArtifacts
    . runBuiltinsArtifacts
    . runNameIdGenArtifacts
    . ignoreHighlightBuilder
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

typeCheckImport :: Import -> Sem r Import
typeCheckImport = return

typeCheckingNew ::
  forall r.
  (Members '[Reader EntryPoint, Error JuvixError, NameIdGen, Reader ModuleTable] r) =>
  Sem (Termination ': r) InternalResult ->
  Sem r InternalTypedResult
typeCheckingNew a = do
  (termin, (res, (normalized, (idens, (funs, r))))) <- runTermination iniTerminationState $ do
    res <- a
    itab <- getInternalModuleTable <$> ask
    let md :: InternalModule
        md = res ^. Internal.resultInternalModule
        itab' :: InternalModuleTable
        itab' = insertInternalModule itab md
        table :: InfoTable
        table = computeCombinedInfoTable itab'
    fmap (res,)
      . runOutputList
      . runState (computeTypesTable itab')
      . runState (computeFunctionsTable itab')
      . runReader table
      . mapError (JuvixError @TypeCheckerError)
      $ checkTable >> checkModule (res ^. Internal.resultModule)
  let md = computeInternalModule idens funs r
  return
    InternalTypedResult
      { _resultInternal = res,
        _resultModule = r,
        _resultInternalModule = md,
        _resultTermination = termin,
        _resultNormalized = HashMap.fromList [(e ^. exampleId, e ^. exampleExpression) | e <- normalized],
        _resultIdenTypes = idens,
        _resultFunctions = funs
      }
