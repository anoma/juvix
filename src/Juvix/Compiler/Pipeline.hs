module Juvix.Compiler.Pipeline
  ( module Juvix.Compiler.Pipeline,
    module Juvix.Compiler.Pipeline.EntryPoint,
    module Juvix.Compiler.Pipeline.Artifacts,
    module Juvix.Compiler.Pipeline.Root,
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
import Juvix.Compiler.Concrete.Data.Scope
import Juvix.Compiler.Concrete.Translation.FromParsed qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver qualified as PathResolver
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context qualified as Scoped
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Core.Translation.Stripped.FromCore qualified as Stripped
import Juvix.Compiler.Internal qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Data.Context qualified as Arity
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as Typed
import Juvix.Compiler.Pipeline.Artifacts
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Root
import Juvix.Compiler.Pipeline.Setup
import Juvix.Compiler.Reg.Data.InfoTable qualified as Reg
import Juvix.Compiler.Reg.Translation.FromAsm qualified as Reg
import Juvix.Prelude

type PipelineEff = '[PathResolver, Reader EntryPoint, Files, NameIdGen, Builtins, Error JuvixError, Embed IO]

type TopPipelineEff = '[PathResolver, Reader EntryPoint, Files, NameIdGen, Builtins, State Artifacts, Error JuvixError, Embed IO]

arityCheckExpression ::
  Members '[Error JuvixError, State Artifacts] r =>
  Path Abs File ->
  Text ->
  Sem r Internal.Expression
arityCheckExpression fp txt = do
  mainScope <- fromJust <$> gets (^. artifactMainModuleScope)
  scopeTable <- gets (^. artifactScopeTable)
  ( runNameIdGenArtifacts
      . runBuiltinsArtifacts
    )
    $ Parser.expressionFromTextSource fp txt
      >>= Scoper.scopeCheckExpression scopeTable mainScope
      >>= Abstract.fromConcreteExpression
      >>= Internal.fromAbstractExpression
      >>= Internal.arityCheckExpression

inferExpression ::
  Members '[Error JuvixError, State Artifacts] r =>
  Path Abs File ->
  Text ->
  Sem r Internal.Expression
inferExpression fp txt =
  ( runNameIdGenArtifacts
      . runBuiltinsArtifacts
  )
    $ arityCheckExpression fp txt
      >>= Internal.inferExpressionType

compileExpression ::
  Members '[Error JuvixError, State Artifacts] r =>
  Path Abs File ->
  Text ->
  Sem r Core.Node
compileExpression fp txt =
  ( runNameIdGenArtifacts
      . runBuiltinsArtifacts
  )
    $ arityCheckExpression fp txt
      >>= Internal.typeCheckExpression
      >>= fromInternalExpression

fromInternalExpression :: Members '[State Artifacts] r => Internal.Expression -> Sem r Core.Node
fromInternalExpression exp = do
  typedTable <- gets (^. artifactInternalTypedTable)
  runReader typedTable
    . tmpCoreInfoTableBuilderArtifacts
    . runFunctionsTableArtifacts
    . readerTypesTableArtifacts
    . runReader Core.initIndexTable
    $ Core.goExpression exp

compileExpressionIO ::
  Members '[State Artifacts, Embed IO] r =>
  Path Abs File ->
  Text ->
  Sem r (Either JuvixError Core.Node)
compileExpressionIO fp txt =
  runError
    . runNameIdGenArtifacts
    . runBuiltinsArtifacts
    $ compileExpression fp txt

inferExpressionIO ::
  Members '[State Artifacts, Embed IO] r =>
  Path Abs File ->
  Text ->
  Sem r (Either JuvixError Internal.Expression)
inferExpressionIO fp txt =
  runError
    . runNameIdGenArtifacts
    . runBuiltinsArtifacts
    $ inferExpression fp txt

--------------------------------------------------------------------------------
-- Workflows
--------------------------------------------------------------------------------

upToParsing ::
  (Members '[Reader EntryPoint, Files, Error JuvixError, NameIdGen, PathResolver] r) =>
  Sem r Parser.ParserResult
upToParsing = entrySetup >> ask >>= Parser.fromSource

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
  Members '[Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, PathResolver] r =>
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

upToCoreTypecheck ::
  (Members '[Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, PathResolver] r) =>
  Sem r Core.CoreResult
upToCoreTypecheck =
  upToCore >>= \r -> Core.toTypechecked (r ^. Core.coreResultTable) >>= \tab -> return r {Core._coreResultTable = tab}

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

-- | It returns `ResolverState` so that we can retrieve the `juvix.yaml` files,
-- which we require for `Scope` tests.
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

corePipelineIO' :: EntryPoint -> IO Artifacts
corePipelineIO' = corePipelineIO defaultGenericOptions

corePipelineIO :: GenericOptions -> EntryPoint -> IO Artifacts
corePipelineIO opts entry = corePipelineIOEither entry >>= mayThrow
  where
    mayThrow :: Either JuvixError r -> IO r
    mayThrow = \case
      Left err -> runM . runReader opts $ printErrorAnsiSafe err >> embed exitFailure
      Right r -> return r

corePipelineIOEither ::
  EntryPoint ->
  IO (Either JuvixError Artifacts)
corePipelineIOEither entry = do
  eith <-
    runM
      . runError
      . runState initialArtifacts
      . runBuiltinsArtifacts
      . runNameIdGenArtifacts
      . runFilesIO
      . runReader entry
      . runPathResolverArtifacts
      $ upToCore
  return $ case eith of
    Left err -> Left err
    Right (art, coreRes) ->
      let typedResult :: Internal.InternalTypedResult
          typedResult =
            coreRes
              ^. Core.coreResultInternalTypedResult

          typesTable :: Typed.TypesTable
          typesTable = typedResult ^. Typed.resultIdenTypes

          functionsTable :: Typed.FunctionsTable
          functionsTable = typedResult ^. Typed.resultFunctions

          typedTable :: Internal.InfoTable
          typedTable = typedResult ^. Typed.resultInfoTable

          coreTable :: Core.InfoTable
          coreTable = coreRes ^. Core.coreResultTable

          scopedResult :: Scoped.ScoperResult
          scopedResult =
            typedResult
              ^. Typed.resultInternalArityResult
                . Arity.resultInternalResult
                . Internal.resultAbstract
                . Abstract.resultScoper

          resultScoperTable :: Scoped.InfoTable
          resultScoperTable = scopedResult ^. Scoped.resultScoperTable

          mainModuleScope_ :: Scope
          mainModuleScope_ = Scoped.mainModuleSope scopedResult
       in Right $
            foldl'
              (flip ($))
              art
              [ set artifactMainModuleScope (Just mainModuleScope_),
                set artifactInternalTypedTable typedTable,
                set artifactCoreTable coreTable,
                set artifactScopeTable resultScoperTable,
                set artifactTypes typesTable,
                set artifactFunctions functionsTable
              ]
  where
    initialArtifacts :: Artifacts
    initialArtifacts =
      Artifacts
        { _artifactMainModuleScope = Nothing,
          _artifactInternalTypedTable = mempty,
          _artifactTypes = mempty,
          _artifactResolver = PathResolver.iniResolverState,
          _artifactNameIdState = allNameIds,
          _artifactFunctions = mempty,
          _artifactCoreTable = Core.emptyInfoTable,
          _artifactScopeTable = Scoped.emptyInfoTable,
          _artifactBuiltins = iniBuiltins
        }
