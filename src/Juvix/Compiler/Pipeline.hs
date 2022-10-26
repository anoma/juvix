module Juvix.Compiler.Pipeline
  ( module Juvix.Compiler.Pipeline,
    module Juvix.Compiler.Pipeline.EntryPoint,
    module Juvix.Compiler.Pipeline.ExpressionContext,
  )
where

import Juvix.Compiler.Abstract.Translation qualified as Abstract
import Juvix.Compiler.Backend.C qualified as C
import Juvix.Compiler.Builtins
import Juvix.Compiler.Concrete qualified as Concrete
import Juvix.Compiler.Concrete.Translation.FromParsed qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Core.Language qualified as Core
import Juvix.Compiler.Core.Translation qualified as Core
import Juvix.Compiler.Internal qualified as Internal
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.ExpressionContext
import Juvix.Compiler.Pipeline.Setup
import Juvix.Prelude

type PipelineEff = '[Reader EntryPoint, Files, NameIdGen, Builtins, Error JuvixError, Embed IO]

arityCheckExpression ::
  Members '[Error JuvixError, NameIdGen, Builtins] r =>
  FilePath ->
  ExpressionContext ->
  Text ->
  Sem r Internal.Expression
arityCheckExpression fp ctx txt =
  Parser.expressionFromTextSource fp txt
    >>= Scoper.scopeCheckExpression (ctx ^. contextScoperTable) (mainModuleScope ctx)
    >>= Abstract.fromConcreteExpression
    >>= Internal.fromAbstractExpression
    >>= Internal.arityCheckExpression (ctx ^. contextInternalResult)

inferExpression ::
  Members '[Error JuvixError, NameIdGen, Builtins] r =>
  FilePath ->
  ExpressionContext ->
  Text ->
  Sem r Internal.Expression
inferExpression fp ctx txt =
  arityCheckExpression fp ctx txt
    >>= Internal.inferExpressionType (ctx ^. contextInternalTypedResult)

compileExpression ::
  Members '[Error JuvixError, NameIdGen, Builtins] r =>
  FilePath ->
  ExpressionContext ->
  Text ->
  Sem r Core.Node
compileExpression fp ctx txt =
  arityCheckExpression fp ctx txt
    >>= Internal.typeCheckExpression (ctx ^. contextInternalTypedResult)
    >>= Core.fromInternalExpression (ctx ^. contextCoreResult)

compileExpressionIO ::
  FilePath ->
  ExpressionContext ->
  BuiltinsState ->
  Text ->
  IO (Either JuvixError Core.Node)
compileExpressionIO fp ctx builtinsState txt =
  runM
    . runError
    . runNameIdGen
    . (fmap snd . runBuiltins builtinsState)
    $ compileExpression fp ctx txt

inferExpressionIO ::
  FilePath ->
  ExpressionContext ->
  BuiltinsState ->
  Text ->
  IO (Either JuvixError Internal.Expression)
inferExpressionIO fp ctx builtinsState txt =
  runM
    . runError
    . runNameIdGen
    . (fmap snd . runBuiltins builtinsState)
    $ inferExpression fp ctx txt

--------------------------------------------------------------------------------
-- Workflows
--------------------------------------------------------------------------------

typecheck ::
  Members PipelineEff r =>
  EntryPoint ->
  Sem r Internal.InternalTypedResult
typecheck =
  do
    Concrete.fromSource
    >=> Abstract.fromConcrete
    >=> Internal.fromAbstract
    >=> Internal.arityChecking
    >=> Internal.typeChecking

compile ::
  Members PipelineEff r =>
  EntryPoint ->
  Sem r C.MiniCResult
compile = typecheck >=> C.fromInternal

upToParsing ::
  Members '[Reader EntryPoint, Files, Error JuvixError, NameIdGen] r =>
  Sem r Parser.ParserResult
upToParsing = entrySetup >>= Parser.fromSource

upToScoping ::
  Members '[Reader EntryPoint, Files, NameIdGen, Error JuvixError] r =>
  Sem r Scoper.ScoperResult
upToScoping = upToParsing >>= Scoper.fromParsed

upToAbstract ::
  Members '[Reader EntryPoint, Files, NameIdGen, Builtins, Error JuvixError] r =>
  Sem r Abstract.AbstractResult
upToAbstract = upToScoping >>= Abstract.fromConcrete

upToInternal ::
  Members '[Reader EntryPoint, Files, NameIdGen, Builtins, Error JuvixError] r =>
  Sem r Internal.InternalResult
upToInternal = upToAbstract >>= Internal.fromAbstract

upToInternalArity ::
  Members '[Reader EntryPoint, Files, NameIdGen, Builtins, Error JuvixError] r =>
  Sem r Internal.InternalArityResult
upToInternalArity = upToInternal >>= Internal.arityChecking

upToInternalTyped ::
  Members '[Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins] r =>
  Sem r Internal.InternalTypedResult
upToInternalTyped = upToInternalArity >>= Internal.typeChecking

upToInternalReachability ::
  Members '[Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins] r =>
  Sem r Internal.InternalTypedResult
upToInternalReachability =
  Internal.filterUnreachable <$> upToInternalTyped

upToCore ::
  Members '[Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins] r =>
  Sem r Core.CoreResult
upToCore =
  upToInternalReachability >>= Core.fromInternal

upToMiniC ::
  Members '[Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins] r =>
  Sem r C.MiniCResult
upToMiniC = upToInternalReachability >>= C.fromInternal

runIOEither :: forall a. BuiltinsState -> EntryPoint -> Sem PipelineEff a -> IO (Either JuvixError (BuiltinsState, a))
runIOEither builtinsState entry =
  runM
    . runError
    . runBuiltins builtinsState
    . runNameIdGen
    . mapError (JuvixError @FilesError)
    . runFilesIO (entry ^. entryPointRoot)
    . runReader entry

runIO :: BuiltinsState -> GenericOptions -> EntryPoint -> Sem PipelineEff a -> IO (BuiltinsState, a)
runIO builtinsState opts entry = runIOEither builtinsState entry >=> mayThrow
  where
    mayThrow :: Either JuvixError r -> IO r
    mayThrow = \case
      Left err -> runM $ runReader opts $ printErrorAnsiSafe err >> embed exitFailure
      Right r -> return r

runIO' :: BuiltinsState -> EntryPoint -> Sem PipelineEff a -> IO (BuiltinsState, a)
runIO' builtinsState = runIO builtinsState defaultGenericOptions
