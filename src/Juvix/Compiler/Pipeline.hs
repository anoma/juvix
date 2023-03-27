module Juvix.Compiler.Pipeline
  ( module Juvix.Compiler.Pipeline,
    module Juvix.Compiler.Pipeline.EntryPoint,
    module Juvix.Compiler.Pipeline.Artifacts,
    module Juvix.Compiler.Pipeline.ExpressionContext,
  )
where

import Juvix.Compiler.Abstract.Translation qualified as Abstract
import Juvix.Compiler.Asm.Error qualified as Asm
import Juvix.Compiler.Asm.Options qualified as Asm
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
import Juvix.Compiler.Core.Language qualified as Core
import Juvix.Compiler.Core.Pipeline qualified as Core
import Juvix.Compiler.Core.Translation qualified as Core
import Juvix.Compiler.Core.Translation.Stripped.FromCore qualified as Stripped
import Juvix.Compiler.Internal qualified as Internal
import Juvix.Compiler.Pipeline.Artifacts
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.ExpressionContext
import Juvix.Compiler.Pipeline.Setup
import Juvix.Compiler.Reg.Data.InfoTable qualified as Reg
import Juvix.Compiler.Reg.Translation.FromAsm qualified as Reg
import Juvix.Prelude

type PipelineEff = '[PathResolver, Reader EntryPoint, Files, NameIdGen, Builtins, Error JuvixError, Embed IO]

arityCheckExpression ::
  Members '[Error JuvixError, NameIdGen, Builtins] r =>
  Path Abs File ->
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
  Path Abs File ->
  ExpressionContext ->
  Text ->
  Sem r Internal.Expression
inferExpression fp ctx txt =
  arityCheckExpression fp ctx txt
    >>= Internal.inferExpressionType (ctx ^. contextInternalTypedResult)

compileExpression ::
  Members '[Error JuvixError, NameIdGen, Builtins] r =>
  Path Abs File ->
  ExpressionContext ->
  Text ->
  Sem r Core.Node
compileExpression fp ctx txt =
  arityCheckExpression fp ctx txt
    >>= Internal.typeCheckExpression (ctx ^. contextInternalTypedResult)
    >>= Core.fromInternalExpression (ctx ^. contextCoreResult)

compileExpressionIO ::
  Path Abs File ->
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
  Path Abs File ->
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
  (Members PipelineEff r) =>
  EntryPoint ->
  Sem r Internal.InternalTypedResult
typecheck =
  do
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
  Sem r C.MiniCResult
upToMiniC = upToAsm >>= asmToMiniC

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

coreToAsm :: Members '[Error JuvixError, Reader EntryPoint] r => Core.InfoTable -> Sem r Asm.InfoTable
coreToAsm = Core.toStripped >=> return . Asm.fromCore . Stripped.fromCore

coreToMiniC :: Members '[Error JuvixError, Reader EntryPoint] r => Core.InfoTable -> Sem r C.MiniCResult
coreToMiniC = coreToAsm >=> asmToMiniC

asmToMiniC :: Members '[Error JuvixError, Reader EntryPoint] r => Asm.InfoTable -> Sem r C.MiniCResult
asmToMiniC = Asm.toReg >=> regToMiniC . Reg.fromAsm

regToMiniC :: Member (Reader EntryPoint) r => Reg.InfoTable -> Sem r C.MiniCResult
regToMiniC tab = do
  e <- ask
  return $ C.fromReg (Backend.getLimits (e ^. entryPointTarget) (e ^. entryPointDebug)) tab

coreToGeb :: Members '[Error JuvixError, Reader EntryPoint] r => Geb.ResultSpec -> Core.InfoTable -> Sem r Geb.Result
coreToGeb spec = Core.toGeb >=> return . uncurry (Geb.toResult spec) . Geb.fromCore

asmToMiniC' :: Members '[Error JuvixError, Reader Asm.Options] r => Asm.InfoTable -> Sem r C.MiniCResult
asmToMiniC' = mapError (JuvixError @Asm.AsmError) . Asm.toReg' >=> regToMiniC' . Reg.fromAsm

regToMiniC' :: Member (Reader Asm.Options) r => Reg.InfoTable -> Sem r C.MiniCResult
regToMiniC' tab = do
  e <- ask
  return $ C.fromReg (e ^. Asm.optLimits) tab

--------------------------------------------------------------------------------
-- Run pipeline
--------------------------------------------------------------------------------

runIOEither :: forall a. BuiltinsState -> EntryPoint -> Sem PipelineEff a -> IO (Either JuvixError (Artifacts, a))
runIOEither builtinsState entry =
  runM
    . runError
    . fmap makeArtifacts
    . runBuiltins builtinsState
    . runNameIdGen
    . runFilesIO
    . runReader entry
    . runPathResolverPipe
  where
    makeArtifacts :: (BuiltinsState, (ResolverState, any)) -> (Artifacts, any)
    makeArtifacts (builtins, (resolver, a)) =
      ( Artifacts
          { _artifactBuiltins = builtins,
            _artifactResolver = resolver
          },
        a
      )

runIO :: BuiltinsState -> GenericOptions -> EntryPoint -> Sem PipelineEff a -> IO (Artifacts, a)
runIO builtinsState opts entry = runIOEither builtinsState entry >=> mayThrow
  where
    mayThrow :: Either JuvixError r -> IO r
    mayThrow = \case
      Left err -> runM $ runReader opts $ printErrorAnsiSafe err >> embed exitFailure
      Right r -> return r

runIO' :: BuiltinsState -> EntryPoint -> Sem PipelineEff a -> IO (Artifacts, a)
runIO' builtinsState = runIO builtinsState defaultGenericOptions
