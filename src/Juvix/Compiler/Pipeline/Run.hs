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
import Juvix.Compiler.Store.Scoped.Language qualified as Scoped
import Juvix.Data.Effect.Git
import Juvix.Data.Effect.Process
import Juvix.Data.Effect.TaggedLock
import Juvix.Prelude

-- | It returns `ResolverState` so that we can retrieve the `juvix.yaml` files,
-- which we require for `Scope` tests.
runIOEither :: forall a r. (Members '[TaggedLock, Embed IO] r) => EntryPoint -> Sem (PipelineEff r) a -> Sem r (Either JuvixError (ResolverState, PipelineResult a))
runIOEither entry = fmap snd . runIOEitherHelper entry

runIOEither' :: forall a r. (Members '[TaggedLock, Embed IO] r) => EntryPoint -> Sem (PipelineEff r) a -> Sem r (Either JuvixError (ResolverState, PipelineResult a))
runIOEither' entry = fmap snd . runIOEitherHelper entry

runPipelineHighlight :: forall a r. (Members '[TaggedLock, Embed IO] r) => EntryPoint -> Sem (PipelineEff r) a -> Sem r HighlightInput
runPipelineHighlight entry = fmap fst . runIOEitherHelper entry

runPipelineHtmlEither :: forall r. (Members '[TaggedLock, Embed IO] r) => EntryPoint -> Sem r (Either JuvixError (Typed.InternalTypedResult, [Typed.InternalTypedResult]))
runPipelineHtmlEither entry = do
  x <- runIOEitherPipeline' entry $ entrySetup defaultDependenciesConfig >> processRecursiveUpToTyped
  return $ mapRight snd $ snd x

runIOEitherHelper :: forall a r. (Members '[TaggedLock, Embed IO] r) => EntryPoint -> Sem (PipelineEff r) a -> Sem r (HighlightInput, (Either JuvixError (ResolverState, PipelineResult a)))
runIOEitherHelper entry a = do
  runIOEitherPipeline' entry $
    entrySetup defaultDependenciesConfig >> processFileUpTo a

runIOEitherPipeline ::
  forall a r.
  (Members '[TaggedLock, Embed IO] r) =>
  EntryPoint ->
  Sem (PipelineEff' r) a ->
  Sem r (Either JuvixError (ResolverState, a))
runIOEitherPipeline entry = fmap snd . runIOEitherPipeline' entry

runIOEitherPipeline' ::
  forall a r.
  (Members '[TaggedLock, Embed IO] r) =>
  EntryPoint ->
  Sem (PipelineEff' r) a ->
  Sem r (HighlightInput, (Either JuvixError (ResolverState, a)))
runIOEitherPipeline' entry a = do
  let hasInternet = not (entry ^. entryPointOffline)
      runPathResolver'
        | mainIsPackageFile entry = runPackagePathResolver' (entry ^. entryPointResolverRoot)
        | otherwise = runPathResolverPipe
  evalInternet hasInternet
    . runHighlightBuilder
    . runJuvixError
    . runFilesIO
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

runIO ::
  forall a r.
  (Members '[TaggedLock, Embed IO] r) =>
  GenericOptions ->
  EntryPoint ->
  Sem (PipelineEff r) a ->
  Sem r (ResolverState, PipelineResult a)
runIO opts entry = runIOEither entry >=> mayThrow
  where
    mayThrow :: (Members '[Embed IO] r') => Either JuvixError x -> Sem r' x
    mayThrow = \case
      Left err -> runReader opts $ printErrorAnsiSafe err >> embed exitFailure
      Right r -> return r

runReplPipelineIO :: EntryPoint -> IO Artifacts
runReplPipelineIO = runReplPipelineIO' defaultGenericOptions

runReplPipelineIO' :: GenericOptions -> EntryPoint -> IO Artifacts
runReplPipelineIO' opts entry = runReplPipelineIOEither entry >>= mayThrow
  where
    mayThrow :: Either JuvixError r -> IO r
    mayThrow = \case
      Left err -> runM . runReader opts $ printErrorAnsiSafe err >> embed exitFailure
      Right r -> return r

runReplPipelineIOEither ::
  EntryPoint ->
  IO (Either JuvixError Artifacts)
runReplPipelineIOEither = runReplPipelineIOEither' LockModePermissive

runReplPipelineIOEither' ::
  LockMode ->
  EntryPoint ->
  IO (Either JuvixError Artifacts)
runReplPipelineIOEither' lockMode entry = do
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
      $ entrySetup defaultDependenciesConfig >> processFileToStoredCore entry
  return $ case eith of
    Left err -> Left err
    Right (art, PipelineResult {..}) ->
      let typedResult :: Internal.InternalTypedResult
          typedResult =
            _pipelineResult
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

          coreModule :: Core.Module
          coreModule = _pipelineResult ^. Core.coreResultModule

          scopedResult :: Scoped.ScoperResult
          scopedResult =
            internalResult
              ^. Internal.resultScoper

          parserResult :: P.ParserResult
          parserResult = scopedResult ^. Scoped.resultParserResult

          resultScoperTable :: InfoTable
          resultScoperTable = Scoped.getCombinedInfoTable (scopedResult ^. Scoped.resultScopedModule)
       in Right $
            appendArtifactsModuleTable _pipelineResultImports $
              Artifacts
                { _artifactMainModuleScope = Just $ scopedResult ^. Scoped.resultScope,
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
                  _artifactModuleTable = mempty
                }
  where
    initialArtifacts :: Artifacts
    initialArtifacts =
      Artifacts
        { _artifactParsing = mempty,
          _artifactMainModuleScope = Nothing,
          _artifactInternalTypedTable = mempty,
          _artifactTerminationState = iniTerminationState,
          _artifactResolver = iniResolverState,
          _artifactNameIdState = genNameIdState defaultModuleId,
          _artifactTypes = mempty,
          _artifactFunctions = mempty,
          _artifactCoreModule = Core.emptyModule,
          _artifactScopeTable = mempty,
          _artifactBuiltins = iniBuiltins,
          _artifactScopeExports = mempty,
          _artifactScoperState = Scoper.iniScoperState mempty,
          _artifactModuleTable = mempty
        }
