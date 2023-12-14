module Juvix.Compiler.Pipeline.Run
  ( module Juvix.Compiler.Pipeline.Run,
    module Juvix.Compiler.Pipeline,
  )
where

import Juvix.Compiler.Builtins
import Juvix.Compiler.Concrete.Data.Highlight
import Juvix.Compiler.Concrete.Data.Scope
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoped
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromSource qualified as P
import Juvix.Compiler.Core.Data.Module qualified as Core
import Juvix.Compiler.Core.Translation.FromInternal.Data qualified as Core
import Juvix.Compiler.Internal.Translation qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Data.Context qualified as Arity
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking qualified as Typed
import Juvix.Compiler.Pipeline
import Juvix.Compiler.Pipeline.Artifacts.PathResolver
import Juvix.Compiler.Pipeline.Driver
import Juvix.Compiler.Pipeline.Loader.PathResolver
import Juvix.Compiler.Pipeline.Package.Loader.Error
import Juvix.Compiler.Pipeline.Package.Loader.EvalEff.IO
import Juvix.Compiler.Pipeline.Package.Loader.PathResolver
import Juvix.Compiler.Pipeline.Setup
import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Compiler.Store.Scoped.Language qualified as Scoped
import Juvix.Data.Effect.Git
import Juvix.Data.Effect.Process
import Juvix.Data.Effect.TaggedLock
import Juvix.Prelude

runPipelineHighlight :: forall r a. (Members '[TaggedLock, Embed IO] r) => EntryPoint -> Sem (PipelineEff r) a -> Sem r HighlightInput
runPipelineHighlight entry = fmap fst . runIOEither entry

-- | It returns `ResolverState` so that we can retrieve the `juvix.yaml` files,
-- which we require for `Scope` tests.
runIOEither :: forall a. EntryPoint -> Sem PipelineEff a -> IO (Either JuvixError (ResolverState, (a, Store.ModuleTable)))
runIOEither entry = fmap snd . runIOEitherHelper entry

runIOEither' :: forall a. LockMode -> EntryPoint -> Sem PipelineEff a -> IO (Either JuvixError (ResolverState, (a, Store.ModuleTable)))
runIOEither' lockMode entry = fmap snd . runIOEitherHelper' lockMode entry

runIOEitherTermination :: forall a. EntryPoint -> Sem (Termination ': PipelineEff) a -> IO (Either JuvixError (ResolverState, (a, Store.ModuleTable)))
runIOEitherTermination entry = fmap snd . runIOEitherHelper entry . evalTermination iniTerminationState

runIOEitherTermination' :: forall a. LockMode -> EntryPoint -> Sem (Termination ': PipelineEff) a -> IO (Either JuvixError (ResolverState, (a, Store.ModuleTable)))
runIOEitherTermination' lockMode entry = fmap snd . runIOEitherHelper' lockMode entry . evalTermination iniTerminationState

runPipelineHighlight :: forall a. EntryPoint -> Sem PipelineEff a -> IO HighlightInput
runPipelineHighlight entry = fmap fst . runIOEitherHelper entry

runIOEitherHelper :: forall a. EntryPoint -> Sem PipelineEff a -> IO (HighlightInput, (Either JuvixError (ResolverState, (a, Store.ModuleTable))))
runIOEitherHelper = runIOEitherHelper' LockModePermissive

runIOEitherHelper' :: forall a. LockMode -> EntryPoint -> Sem PipelineEff a -> IO (HighlightInput, (Either JuvixError (ResolverState, (a, Store.ModuleTable))))
runIOEitherHelper' lockMode entry a = do
  runIOEitherPipeline' lockMode entry $
    entrySetup defaultDependenciesConfig >> processFileUpTo a

runIOEitherPipeline ::
  forall a.
  EntryPoint ->
  Sem PipelineEff' a ->
  IO (Either JuvixError (ResolverState, a))
runIOEitherPipeline entry = fmap snd . runIOEitherPipeline' LockModePermissive entry

runIOEitherPipeline' ::
  forall a.
  LockMode ->
  EntryPoint ->
  Sem PipelineEff' a ->
  IO (HighlightInput, (Either JuvixError (ResolverState, a)))
runIOEitherPipeline' lockMode entry a = do
  let hasInternet = not (entry ^. entryPointOffline)
      runPathResolver'
        | mainIsPackageFile entry = runPackagePathResolver' (entry ^. entryPointResolverRoot)
        | otherwise = runPathResolverPipe
  evalInternet hasInternet
    . runHighlightBuilder
    . runJuvixError
    . runFilesIO
    . runTaggedLock lockMode
    . runReader entry
    . runLogIO
    . runProcessIO
    . mapError (JuvixError @GitProcessError)
    . runGitProcess
    . mapError (JuvixError @DependencyError)
    . mapError (JuvixError @PackageLoaderError)
    . runEvalFileEffIO
    . runPathResolver'
    $ a

mainIsPackageFile :: EntryPoint -> Bool
mainIsPackageFile entry = case entry ^. entryPointModulePath of
  Just p -> p == mkPackagePath (entry ^. entryPointResolverRoot)
  Nothing -> False

runIOLockMode :: LockMode -> GenericOptions -> EntryPoint -> Sem PipelineEff a -> IO (ResolverState, (a, Store.ModuleTable))
runIOLockMode lockMode opts entry = runIOEither' lockMode entry >=> mayThrow
  where
    mayThrow :: (Members '[Embed IO] r') => Either JuvixError x -> Sem r' x
    mayThrow = \case
      Left err -> runM . runReader opts $ printErrorAnsiSafe err >> embed exitFailure
      Right r -> return r

runIO :: GenericOptions -> EntryPoint -> Sem PipelineEff a -> IO (ResolverState, (a, Store.ModuleTable))
runIO opts entry = runIOEither entry >=> mayThrow
  where
    mayThrow :: Either JuvixError r -> IO r
    mayThrow = \case
      Left err -> runM . runReader opts $ printErrorAnsiSafe err >> embed exitFailure
      Right r -> return r

runIOExclusive :: EntryPoint -> Sem PipelineEff a -> IO (ResolverState, (a, Store.ModuleTable))
runIOExclusive = runIOLockMode LockModeExclusive defaultGenericOptions

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
corePipelineIOEither = corePipelineIOEither' LockModePermissive

corePipelineIOEither' ::
  LockMode ->
  EntryPoint ->
  IO (Either JuvixError Artifacts)
corePipelineIOEither' lockMode entry = do
  let hasInternet = not (entry ^. entryPointOffline)
      runPathResolver'
        | mainIsPackageFile entry = runPackagePathResolverArtifacts (entry ^. entryPointResolverRoot)
        | otherwise = runPathResolverArtifacts
  eith <-
    runFinal
      . resourceToIOFinal
      . embedToFinal @IO
      . evalInternet hasInternet
      . ignoreHighlightBuilder
      . runError
      . runState initialArtifacts
      . runBuiltinsArtifacts
      . runNameIdGenArtifacts
      . runFilesIO
      . runReader entry
      . runTaggedLock lockMode
      . runLogIO
      . mapError (JuvixError @GitProcessError)
      . runProcessIO
      . runGitProcess
      . mapError (JuvixError @DependencyError)
      . mapError (JuvixError @PackageLoaderError)
      . runEvalFileEffIO
      . runPathResolver'
      $ processFileToStoredCore entry
  return $ case eith of
    Left err -> Left err
    Right (art, (coreRes, mtab)) ->
      let typedResult :: Internal.InternalTypedResult
          typedResult =
            coreRes
              ^. Core.coreResultInternalTypedResult

          typesTable :: Typed.TypesTable
          typesTable = typedResult ^. Typed.resultIdenTypes

          functionsTable :: Typed.FunctionsTable
          functionsTable = typedResult ^. Typed.resultFunctions

          typedTable :: Internal.InfoTable
          typedTable = typedResult ^. Typed.resultInternalModule . Typed.internalModuleInfoTable

          internalResult :: Internal.InternalResult
          internalResult =
            typedResult
              ^. Typed.resultInternal
                . Arity.resultInternal

          coreModule :: Core.Module
          coreModule = coreRes ^. Core.coreResultModule

          scopedResult :: Scoped.ScoperResult
          scopedResult =
            internalResult
              ^. Internal.resultScoper

          parserResult :: P.ParserResult
          parserResult = scopedResult ^. Scoped.resultParserResult

          resultScoperTable :: InfoTable
          resultScoperTable = scopedResult ^. Scoped.resultScopedModule . Scoped.scopedModuleInfoTable
       in Right $
            Artifacts
              { _artifactMainModuleScope = Nothing,
                _artifactParsing = parserResult ^. P.resultParserState,
                _artifactInternalTypedTable = typedTable,
                _artifactTerminationState = typedResult ^. Typed.resultTermination,
                _artifactCoreModule = coreModule,
                _artifactScopeTable = resultScoperTable,
                _artifactScopeExports = scopedResult ^. Scoped.resultExports,
                _artifactTypes = typesTable,
                _artifactFunctions = functionsTable,
                _artifactScoperState = scopedResult ^. Scoped.resultScoperState,
                _artifactResolver = art ^. artifactResolver,
                _artifactBuiltins = art ^. artifactBuiltins,
                _artifactNameIdState = art ^. artifactNameIdState,
                _artifactModuleTable = mtab
              }
  where
    initialArtifacts :: Artifacts
    initialArtifacts =
      Artifacts
        { _artifactParsing = mempty,
          _artifactMainModuleScope = Nothing,
          _artifactInternalTypedTable = mempty,
          _artifactTypes = mempty,
          _artifactTerminationState = iniTerminationState,
          _artifactResolver = iniResolverState,
          _artifactNameIdState = genNameIdState defaultModuleId,
          _artifactFunctions = mempty,
          _artifactCoreModule = Core.emptyModule,
          _artifactScopeTable = mempty,
          _artifactBuiltins = iniBuiltins,
          _artifactScopeExports = mempty,
          _artifactScoperState = Scoper.iniScoperState mempty,
          _artifactModuleTable = mempty
        }
