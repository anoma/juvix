module Juvix.Compiler.Pipeline
  ( module Juvix.Compiler.Pipeline,
    module Juvix.Compiler.Pipeline.EntryPoint,
    module Juvix.Compiler.Pipeline.Artifacts,
    module Juvix.Compiler.Pipeline.Root,
  )
where

import Juvix.Compiler.Asm.Error qualified as Asm
import Juvix.Compiler.Asm.Options qualified as Asm
import Juvix.Compiler.Asm.Pipeline qualified as Asm
import Juvix.Compiler.Asm.Translation.FromCore qualified as Asm
import Juvix.Compiler.Backend qualified as Backend
import Juvix.Compiler.Backend.C qualified as C
import Juvix.Compiler.Backend.Geb qualified as Geb
import Juvix.Compiler.Backend.VampIR.Translation qualified as VampIR
import Juvix.Compiler.Builtins
import Juvix.Compiler.Concrete.Data.Highlight.Input
import Juvix.Compiler.Concrete.Data.ParsedInfoTableBuilder.BuilderState qualified as Concrete
import Juvix.Compiler.Concrete.Data.Scope
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromParsed qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver qualified as PathResolver
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context qualified as Scoped
import Juvix.Compiler.Concrete.Translation.FromSource qualified as P
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Core.Translation.Stripped.FromCore qualified as Stripped
import Juvix.Compiler.Internal qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Data.Context qualified as Arity
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as Typed
import Juvix.Compiler.Pipeline.Artifacts
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Root
import Juvix.Compiler.Pipeline.Setup
import Juvix.Compiler.Reg.Data.InfoTable qualified as Reg
import Juvix.Compiler.Reg.Translation.FromAsm qualified as Reg
import Juvix.Data.Effect.Git
import Juvix.Data.Effect.Process
import Juvix.Prelude

type PipelineEff = '[PathResolver, Error DependencyError, GitClone, Error GitProcessError, Process, Log, Reader EntryPoint, Files, NameIdGen, Builtins, Error JuvixError, HighlightBuilder, Internet, Embed IO]

type TopPipelineEff = '[PathResolver, Error DependencyError, GitClone, Error GitProcessError, Process, Log, Reader EntryPoint, Files, NameIdGen, Builtins, State Artifacts, Error JuvixError, HighlightBuilder, Embed IO]

--------------------------------------------------------------------------------
-- Workflows
--------------------------------------------------------------------------------

upToParsing ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, Error JuvixError, NameIdGen, GitClone, PathResolver] r) =>
  Sem r Parser.ParserResult
upToParsing = entrySetup >> ask >>= Parser.fromSource

upToScoping ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Error JuvixError, GitClone, PathResolver] r) =>
  Sem r Scoper.ScoperResult
upToScoping = upToParsing >>= Scoper.fromParsed

upToInternal ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Builtins, Error JuvixError, GitClone, PathResolver, Termination] r) =>
  Sem r Internal.InternalResult
upToInternal = upToScoping >>= Internal.fromConcrete

upToInternalArity ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Builtins, Error JuvixError, GitClone, PathResolver, Termination] r) =>
  Sem r Internal.InternalArityResult
upToInternalArity = upToInternal >>= Internal.arityChecking

upToInternalTyped ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, GitClone, PathResolver] r) =>
  Sem r Internal.InternalTypedResult
upToInternalTyped = Internal.typeChecking upToInternalArity

upToInternalReachability ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, GitClone, PathResolver] r) =>
  Sem r Internal.InternalTypedResult
upToInternalReachability =
  upToInternalTyped >>= Internal.filterUnreachable

upToCore ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, GitClone, PathResolver] r) =>
  Sem r Core.CoreResult
upToCore = upToInternalReachability >>= Core.fromInternal

upToAsm ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, GitClone, PathResolver] r) =>
  Sem r Asm.InfoTable
upToAsm =
  upToCore >>= \Core.CoreResult {..} -> coreToAsm _coreResultTable

upToMiniC ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, GitClone, PathResolver] r) =>
  Sem r C.MiniCResult
upToMiniC = upToAsm >>= asmToMiniC

upToVampIR ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, GitClone, PathResolver] r) =>
  Sem r VampIR.Result
upToVampIR =
  upToCore >>= \Core.CoreResult {..} -> coreToVampIR _coreResultTable

upToGeb ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, GitClone, PathResolver] r) =>
  Geb.ResultSpec ->
  Sem r Geb.Result
upToGeb spec =
  upToCore >>= \Core.CoreResult {..} -> coreToGeb spec _coreResultTable

upToCoreTypecheck ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, GitClone, PathResolver] r) =>
  Sem r Core.CoreResult
upToCoreTypecheck =
  upToCore >>= \r -> Core.toTypechecked (r ^. Core.coreResultTable) >>= \tab -> return r {Core._coreResultTable = tab}

upToEval ::
  (Members '[HighlightBuilder, Reader EntryPoint, Files, NameIdGen, Error JuvixError, Builtins, GitClone, PathResolver] r) =>
  Sem r Core.CoreResult
upToEval =
  upToCore >>= \r -> Core.toEval (r ^. Core.coreResultTable) >>= \tab -> return r {Core._coreResultTable = tab}

--------------------------------------------------------------------------------
-- Internal workflows
--------------------------------------------------------------------------------

coreToAsm :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.InfoTable -> Sem r Asm.InfoTable
coreToAsm = Core.toStripped >=> return . Asm.fromCore . Stripped.fromCore

coreToMiniC :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.InfoTable -> Sem r C.MiniCResult
coreToMiniC = coreToAsm >=> asmToMiniC

asmToMiniC :: (Members '[Error JuvixError, Reader EntryPoint] r) => Asm.InfoTable -> Sem r C.MiniCResult
asmToMiniC = Asm.toReg >=> regToMiniC . Reg.fromAsm

regToMiniC :: (Member (Reader EntryPoint) r) => Reg.InfoTable -> Sem r C.MiniCResult
regToMiniC tab = do
  e <- ask
  return $ C.fromReg (Backend.getLimits (e ^. entryPointTarget) (e ^. entryPointDebug)) tab

coreToGeb :: (Members '[Error JuvixError, Reader EntryPoint] r) => Geb.ResultSpec -> Core.InfoTable -> Sem r Geb.Result
coreToGeb spec = Core.toGeb >=> return . uncurry (Geb.toResult spec) . Geb.fromCore

coreToVampIR :: (Members '[Error JuvixError, Reader EntryPoint] r) => Core.InfoTable -> Sem r VampIR.Result
coreToVampIR = Core.toVampIR >=> return . VampIR.toResult . VampIR.fromCore

asmToMiniC' :: (Members '[Error JuvixError, Reader Asm.Options] r) => Asm.InfoTable -> Sem r C.MiniCResult
asmToMiniC' = mapError (JuvixError @Asm.AsmError) . Asm.toReg' >=> regToMiniC' . Reg.fromAsm

regToMiniC' :: (Member (Reader Asm.Options) r) => Reg.InfoTable -> Sem r C.MiniCResult
regToMiniC' tab = do
  e <- ask
  return $ C.fromReg (e ^. Asm.optLimits) tab

coreToVampIR' :: (Members '[Error JuvixError, Reader Core.CoreOptions] r) => Core.InfoTable -> Sem r VampIR.Result
coreToVampIR' = Core.toVampIR' >=> return . VampIR.toResult . VampIR.fromCore

--------------------------------------------------------------------------------
-- Run pipeline
--------------------------------------------------------------------------------

-- | It returns `ResolverState` so that we can retrieve the `juvix.yaml` files,
-- which we require for `Scope` tests.
runIOEither :: forall a. EntryPoint -> Sem PipelineEff a -> IO (Either JuvixError (ResolverState, a))
runIOEither entry = fmap snd . runIOEitherHelper entry

runIOEitherTermination :: forall a. EntryPoint -> Sem (Termination ': PipelineEff) a -> IO (Either JuvixError (ResolverState, a))
runIOEitherTermination entry = fmap snd . runIOEitherHelper entry . evalTermination iniTerminationState

runPipelineHighlight :: forall a. EntryPoint -> Sem PipelineEff a -> IO HighlightInput
runPipelineHighlight entry = fmap fst . runIOEitherHelper entry

runIOEitherHelper :: forall a. EntryPoint -> Sem PipelineEff a -> IO (HighlightInput, (Either JuvixError (ResolverState, a)))
runIOEitherHelper entry = do
  let hasInternet = not (entry ^. entryPointOffline)
  runM
    . evalInternet hasInternet
    . runHighlightBuilder
    . runJuvixError
    . evalTopBuiltins
    . evalTopNameIdGen
    . runFilesIO
    . runReader entry
    . runLogIO
    . runProcessIO
    . mapError (JuvixError @GitProcessError)
    . runGitProcess
    . mapError (JuvixError @DependencyError)
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
  let hasInternet = not (entry ^. entryPointOffline)
  eith <-
    runM
      . evalInternet hasInternet
      . ignoreHighlightBuilder
      . runError
      . runState initialArtifacts
      . runBuiltinsArtifacts
      . runNameIdGenArtifacts
      . runFilesIO
      . runReader entry
      . runLogIO
      . mapError (JuvixError @GitProcessError)
      . runProcessIO
      . runGitProcess
      . mapError (JuvixError @DependencyError)
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

          internalResult :: Internal.InternalResult
          internalResult =
            typedResult
              ^. Typed.resultInternalArityResult
                . Arity.resultInternalResult

          coreTable :: Core.InfoTable
          coreTable = coreRes ^. Core.coreResultTable

          scopedResult :: Scoped.ScoperResult
          scopedResult =
            internalResult
              ^. Internal.resultScoper

          parserResult :: P.ParserResult
          parserResult = scopedResult ^. Scoped.resultParserResult

          resultScoperTable :: Scoped.InfoTable
          resultScoperTable = scopedResult ^. Scoped.resultScoperTable

          mainModuleScope_ :: Scope
          mainModuleScope_ = Scoped.mainModuleSope scopedResult
       in Right $
            Artifacts
              { _artifactMainModuleScope = Just mainModuleScope_,
                _artifactParsing = parserResult ^. P.resultBuilderState,
                _artifactInternalModuleCache = internalResult ^. Internal.resultModulesCache,
                _artifactInternalTypedTable = typedTable,
                _artifactTerminationState = typedResult ^. Typed.resultTermination,
                _artifactCoreTable = coreTable,
                _artifactScopeTable = resultScoperTable,
                _artifactScopeExports = scopedResult ^. Scoped.resultExports,
                _artifactTypes = typesTable,
                _artifactFunctions = functionsTable,
                _artifactScoperState = scopedResult ^. Scoped.resultScoperState,
                _artifactResolver = art ^. artifactResolver,
                _artifactBuiltins = art ^. artifactBuiltins,
                _artifactNameIdState = art ^. artifactNameIdState
              }
  where
    initialArtifacts :: Artifacts
    initialArtifacts =
      Artifacts
        { _artifactParsing = Concrete.iniState,
          _artifactMainModuleScope = Nothing,
          _artifactInternalTypedTable = mempty,
          _artifactTypes = mempty,
          _artifactTerminationState = iniTerminationState,
          _artifactResolver = PathResolver.iniResolverState,
          _artifactNameIdState = allNameIds,
          _artifactFunctions = mempty,
          _artifactCoreTable = Core.emptyInfoTable,
          _artifactScopeTable = Scoped.emptyInfoTable,
          _artifactBuiltins = iniBuiltins,
          _artifactScopeExports = mempty,
          _artifactInternalModuleCache = Internal.ModulesCache mempty,
          _artifactScoperState = Scoper.iniScoperState
        }
