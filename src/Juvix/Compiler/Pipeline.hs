module Juvix.Compiler.Pipeline
  ( module Juvix.Compiler.Pipeline,
    module Juvix.Compiler.Pipeline.EntryPoint,
    module Juvix.Compiler.Pipeline.Artifacts,
    module Juvix.Compiler.Pipeline.ExpressionContext,
  )
where

import Juvix.Compiler.Abstract.Translation qualified as Abstract
import Juvix.Compiler.Asm.Pipeline qualified as Asm
import Juvix.Compiler.Asm.Translation.FromCore qualified as Asm
import Juvix.Compiler.Backend qualified as Backend
import Juvix.Compiler.Backend.C qualified as C
import Juvix.Compiler.Backend.Geb qualified as Geb
import Juvix.Compiler.Builtins
import Juvix.Compiler.Concrete qualified as Concrete
import Juvix.Compiler.Concrete.Translation.FromParsed qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Core.Translation.Stripped.FromCore qualified as Stripped
import Juvix.Compiler.Internal qualified as Internal
import Juvix.Compiler.Pipeline.Artifacts
import Juvix.Compiler.Pipeline.Artifacts (runBuiltinsArtifacts, runNameIdGenArtifacts)
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.ExpressionContext
import Juvix.Compiler.Pipeline.Setup
import Juvix.Compiler.Reg.Data.InfoTable qualified as Reg
import Juvix.Compiler.Reg.Translation.FromAsm qualified as Reg
import Juvix.Prelude

type PipelineEff = '[PathResolver, Reader EntryPoint, Files, NameIdGen, Builtins, Error JuvixError, Embed IO]

arityCheckExpression ::
  Members '[Error JuvixError, State Artifacts] r =>
  Path Abs File ->
  ExpressionContext ->
  Text ->
  Sem r Internal.Expression
arityCheckExpression fp ctx txt =
  ( runNameIdGenArtifacts
      . runBuiltinsArtifacts
  )
    $ Parser.expressionFromTextSource fp txt
      >>= Scoper.scopeCheckExpression (ctx ^. contextScoperTable) (mainModuleScope ctx)
      >>= Abstract.fromConcreteExpression
      >>= Internal.fromAbstractExpression
      >>= Internal.arityCheckExpression (ctx ^. contextInternalResult)

inferExpression ::
  Members '[Error JuvixError, State Artifacts] r =>
  Path Abs File ->
  ExpressionContext ->
  Text ->
  Sem r Internal.Expression
inferExpression fp ctx txt =
  ( runNameIdGenArtifacts
      . runBuiltinsArtifacts
  )
    $ arityCheckExpression fp ctx txt
      >>= Internal.inferExpressionType (ctx ^. contextInternalTypedResult)

compileExpression ::
  Members '[Error JuvixError, State Artifacts] r =>
  Path Abs File ->
  ExpressionContext ->
  Text ->
  Sem r Core.Node
compileExpression fp ctx txt =
  ( runNameIdGenArtifacts
      . runBuiltinsArtifacts
  )
    $ arityCheckExpression fp ctx txt
      >>= Internal.typeCheckExpression (ctx ^. contextInternalTypedResult)
      >>= (\typed -> trace ("TYPED " <> Internal.ppTrace typed) $ Core.fromInternalExpression (ctx ^. contextCoreResult) typed)

compileExpressionIO ::
  Members '[Error JuvixError, State Artifacts, Embed IO] r =>
  Path Abs File ->
  ExpressionContext ->
  Text ->
  Sem r (Either JuvixError Core.Node)
compileExpressionIO fp ctx txt =
  runError
    . runNameIdGenArtifacts
    . runBuiltinsArtifacts
    $ compileExpression fp ctx txt

inferExpressionIO ::
  Members '[State Artifacts, Embed IO] r =>
  Path Abs File ->
  ExpressionContext ->
  Text ->
  Sem r (Either JuvixError Internal.Expression)
inferExpressionIO fp ctx txt =
  runError
    . runNameIdGenArtifacts
    . runBuiltinsArtifacts
    $ inferExpression fp ctx txt

--------------------------------------------------------------------------------
-- Workflows
--------------------------------------------------------------------------------

typecheck ::
  (Members PipelineEff r) =>
  EntryPoint ->
  Sem r Internal.InternalTypedResult
typecheck =
  Concrete.fromSource
    >=> Abstract.fromConcrete
    >=> Internal.fromAbstract
    >=> Internal.arityChecking
    >=> Internal.typeChecking

upToParsing ::
  (Members '[Reader EntryPoint, Files, Error JuvixError, NameIdGen, PathResolver] r) =>
  Sem r Parser.ParserResult
upToParsing = entrySetup >>= Parser.fromSource

upToScoping ::
  (Members '[Reader EntryPoint, Files, NameIdGen, Error JuvixError, PathResolver] r) =>
  Sem r Scoper.ScoperResult
upToScoping = upToParsing >>= Scoper.fromParsed

upToAbstract ::
  (Members '[Reader EntryPoint, Files, NameIdGen, Builtins, Error JuvixError, PathResolver] r) =>
  Sem r Abstract.AbstractResult
upToAbstract = upToScoping >>= Abstract.fromConcrete

upToInternal ::
  (Members '[Reader EntryPoint, Files, NameIdGen, Builtins, Error JuvixError, PathResolver] r) =>
  Sem r Internal.InternalResult
upToInternal = upToAbstract >>= Internal.fromAbstract

upToInternalArity ::
  (Members '[Reader EntryPoint, Files, NameIdGen, Builtins, Error JuvixError, PathResolver] r) =>
  Sem r Internal.InternalArityResult
upToInternalArity = upToInternal >>= Internal.arityChecking

upToInternalTyped ::
  (Members '[Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, PathResolver] r) =>
  Sem r Internal.InternalTypedResult
upToInternalTyped = upToInternalArity >>= Internal.typeChecking

upToInternalReachability ::
  (Members '[Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, PathResolver] r) =>
  Sem r Internal.InternalTypedResult
upToInternalReachability =
  Internal.filterUnreachable <$> upToInternalTyped

upToCore ::
  (Members '[Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, PathResolver] r) =>
  Sem r Core.CoreResult
upToCore =
  upToInternalReachability >>= Core.fromInternal

upToAsm ::
  (Members '[Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, PathResolver] r) =>
  Sem r Asm.InfoTable
upToAsm =
  upToCore >>= \Core.CoreResult {..} -> coreToAsm _coreResultTable

upToMiniC ::
  (Members '[Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, PathResolver] r) =>
  Asm.Options ->
  Sem r C.MiniCResult
upToMiniC opts =
  upToAsm >>= asmToMiniC opts

upToGeb ::
  (Members '[Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, PathResolver] r) =>
  Geb.ResultSpec ->
  Sem r Geb.Result
upToGeb spec =
  upToCore >>= \Core.CoreResult {..} -> coreToGeb spec _coreResultTable

upToEval ::
  (Members '[Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, PathResolver] r) =>
  Sem r Core.CoreResult
upToEval =
  upToCore >>= \r -> Core.toEval (r ^. Core.coreResultTable) >>= \tab -> return r {Core._coreResultTable = tab}

--------------------------------------------------------------------------------
-- Internal workflows
--------------------------------------------------------------------------------

coreToAsm :: Member (Error JuvixError) r => Core.InfoTable -> Sem r Asm.InfoTable
coreToAsm = Core.toStripped >=> return . Asm.fromCore . Stripped.fromCore

coreToMiniC :: (Member (Error JuvixError) r) => Asm.Options -> Core.InfoTable -> Sem r C.MiniCResult
coreToMiniC opts = coreToAsm >=> asmToMiniC opts

asmToMiniC :: Member (Error JuvixError) r => Asm.Options -> Asm.InfoTable -> Sem r C.MiniCResult
asmToMiniC opts = Asm.toReg opts >=> regToMiniC (opts ^. Asm.optLimits) . Reg.fromAsm

regToMiniC :: Backend.Limits -> Reg.InfoTable -> Sem r C.MiniCResult
regToMiniC lims = return . C.fromReg lims

coreToGeb :: Member (Error JuvixError) r => Geb.ResultSpec -> Core.InfoTable -> Sem r Geb.Result
coreToGeb spec = Core.toGeb >=> return . uncurry (Geb.toResult spec) . Geb.fromCore

--------------------------------------------------------------------------------
-- Run pipeline
--------------------------------------------------------------------------------

-- | It returns `ResolverState` so that we can retrieve the `juvix.yaml` files,
-- which we require for `Sope` tests.
runIOEither :: forall a. EntryPoint -> Sem PipelineEff a -> IO (Either JuvixError (ResolverState, a))
runIOEither entry =
  runM
    . runError
    . evalTopBuiltins
    . evalTopNameIdGen
    . runFilesIO
    . runReader entry
    . runPathResolverPipe

runIO :: GenericOptions -> EntryPoint -> Sem PipelineEff a -> IO (ResolverState, a)
runIO opts entry = runIOEither entry >=> mayThrow
  where
    mayThrow :: Either JuvixError r -> IO r
    mayThrow = \case
      Left err -> runM . runReader opts $ printErrorAnsiSafe err >> embed exitFailure
      Right r -> return r

runIO' :: EntryPoint -> Sem PipelineEff a -> IO (ResolverState, a)
runIO' = runIO defaultGenericOptions
