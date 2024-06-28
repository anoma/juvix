module Juvix.Compiler.Pipeline.Driver
  ( module Juvix.Compiler.Pipeline.Driver.Data,
    ModuleInfoCache,
    JvoCache,
    evalJvoCache,
    processFileUpTo,
    evalModuleInfoCache,
    evalModuleInfoCacheSetup,
    processFileToStoredCore,
    processFileUpToParsing,
    processModule,
    processImport,
    processRecursiveUpToTyped,
    processImports,
    processModuleToStoredCore,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Concrete.Data.Highlight
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping (getModuleId)
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Concrete.Translation.FromSource.Data.ParserState (parserStateImports)
import Juvix.Compiler.Concrete.Translation.FromSource.Data.ParserState qualified as Parser
import Juvix.Compiler.Concrete.Translation.FromSource.TopModuleNameChecker
import Juvix.Compiler.Core.Data.Module qualified as Core
import Juvix.Compiler.Core.Translation.FromInternal.Data.Context qualified as Core
import Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as InternalTyped
import Juvix.Compiler.Internal.Translation.FromInternal.Data (InternalTypedResult)
import Juvix.Compiler.Pipeline
import Juvix.Compiler.Pipeline.Driver.Data
import Juvix.Compiler.Pipeline.JvoCache
import Juvix.Compiler.Pipeline.Loader.PathResolver
import Juvix.Compiler.Pipeline.ModuleInfoCache
import Juvix.Compiler.Store.Core.Extra
import Juvix.Compiler.Store.Extra qualified as Store
import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Compiler.Store.Options qualified as StoredModule
import Juvix.Compiler.Store.Options qualified as StoredOptions
import Juvix.Data.CodeAnn
import Juvix.Data.SHA256 qualified as SHA256
import Juvix.Extra.Serialize qualified as Serialize
import Juvix.Prelude
import Path.Posix qualified as Path

processModule ::
  (Members '[ModuleInfoCache] r) =>
  EntryIndex ->
  Sem r (PipelineResult Store.ModuleInfo)
processModule = cacheGet

evalModuleInfoCache ::
  forall r a.
  (Members '[TaggedLock, TopModuleNameChecker, Error JuvixError, Files, PathResolver] r) =>
  Sem (ModuleInfoCache ': JvoCache ': r) a ->
  Sem r a
evalModuleInfoCache = evalJvoCache . evalCacheEmpty processModuleCacheMiss

-- | Used for parallel compilation
evalModuleInfoCacheSetup ::
  forall r a.
  (Members '[TaggedLock, TopModuleNameChecker, Error JuvixError, Files, PathResolver] r) =>
  (EntryIndex -> Sem (ModuleInfoCache ': JvoCache ': r) ()) ->
  Sem (ModuleInfoCache ': JvoCache ': r) a ->
  Sem r a
evalModuleInfoCacheSetup setup = evalJvoCache . evalCacheEmptySetup setup processModuleCacheMiss

processModuleCacheMiss ::
  forall r.
  ( Members
      '[ ModuleInfoCache,
         TaggedLock,
         TopModuleNameChecker,
         Error JuvixError,
         Files,
         JvoCache,
         PathResolver
       ]
      r
  ) =>
  EntryIndex ->
  Sem r (PipelineResult Store.ModuleInfo)
processModuleCacheMiss entryIx = do
  let buildDir = resolveAbsBuildDir root (entry ^. entryPointBuildDir)
      sourcePath = fromJust (entry ^. entryPointModulePath)
      relPath =
        fromJust
          . replaceExtension ".jvo"
          . fromJust
          $ stripProperPrefix $(mkAbsDir "/") sourcePath
      absPath = buildDir Path.</> relPath
  sha256 <- SHA256.digestFile sourcePath
  m :: Maybe Store.ModuleInfo <- loadFromFile absPath
  case m of
    Just info
      | info ^. Store.moduleInfoSHA256 == sha256
          && info ^. Store.moduleInfoOptions == opts
          && info ^. Store.moduleInfoFieldSize == entry ^. entryPointFieldSize -> do
          CompileResult {..} <- runReader entry (processImports (info ^. Store.moduleInfoImports))
          if
              | _compileResultChanged ->
                  recompile sha256 absPath
              | otherwise ->
                  return
                    PipelineResult
                      { _pipelineResult = info,
                        _pipelineResultImports = _compileResultModuleTable,
                        _pipelineResultChanged = False
                      }
    _ ->
      recompile sha256 absPath
  where
    entry = entryIx ^. entryIxEntry
    root = entry ^. entryPointRoot
    opts = StoredModule.fromEntryPoint entry

    recompile :: Text -> Path Abs File -> Sem r (PipelineResult Store.ModuleInfo)
    recompile sha256 absPath = do
      res <- processModuleToStoredCore sha256 entry
      Serialize.saveToFile absPath (res ^. pipelineResult)
      return res

processRecursiveUpToTyped ::
  forall r.
  ( Members
      '[ Reader EntryPoint,
         TopModuleNameChecker,
         TaggedLock,
         HighlightBuilder,
         Error JuvixError,
         Files,
         PathResolver,
         ModuleInfoCache
       ]
      r
  ) =>
  Sem r (InternalTypedResult, [InternalTypedResult])
processRecursiveUpToTyped = do
  entry <- ask
  PipelineResult {..} <- processFileUpToParsing entry
  let imports = HashMap.keys (_pipelineResultImports ^. Store.moduleTable)
  ms <- forM imports $ \imp ->
    withPathFile imp goImport
  let pkg = entry ^. entryPointPackage
  mid <- runReader pkg (getModuleId (_pipelineResult ^. Parser.resultModule . modulePath . to topModulePathKey))
  a <-
    evalTopNameIdGen mid
      . runReader _pipelineResultImports
      . runReader _pipelineResult
      $ upToInternalTyped
  return (a, ms)
  where
    goImport :: ImportNode -> Sem r InternalTypedResult
    goImport node = do
      entry <- ask
      let entry' =
            entry
              { _entryPointStdin = Nothing,
                _entryPointModulePath = Just (node ^. importNodeAbsFile)
              }
      (^. pipelineResult) <$> runReader entry' (processFileUpTo upToInternalTyped)

processImport ::
  forall r.
  (Members '[ModuleInfoCache, Reader EntryPoint, Error JuvixError, Files, PathResolver] r) =>
  TopModulePath ->
  Sem r (PipelineResult Store.ModuleInfo)
processImport p = withPathFile p getCachedImport
  where
    getCachedImport :: ImportNode -> Sem r (PipelineResult Store.ModuleInfo)
    getCachedImport node = do
      b <- supportsParallel
      eix <- mkEntryIndex node
      if
          | b -> do
              res <- cacheGetResult eix
              return (res ^. cacheResult)
          | otherwise -> processModule eix

processFileUpToParsing ::
  forall r.
  (Members '[ModuleInfoCache, Reader EntryPoint, HighlightBuilder, TopModuleNameChecker, Error JuvixError, Files, PathResolver] r) =>
  EntryPoint ->
  Sem r (PipelineResult Parser.ParserResult)
processFileUpToParsing entry = do
  res <- runReader entry upToParsing
  let imports :: [Import 'Parsed] = res ^. Parser.resultParserState . Parser.parserStateImports
  mtab <- (^. compileResultModuleTable) <$> processImports (map (^. importModulePath) imports)
  return
    PipelineResult
      { _pipelineResult = res,
        _pipelineResultImports = mtab,
        _pipelineResultChanged = True
      }

processFileUpTo ::
  forall r a.
  (Members '[Reader EntryPoint, Error JuvixError, TopModuleNameChecker, PathResolver, Files, HighlightBuilder, ModuleInfoCache] r) =>
  Sem (Reader Parser.ParserResult ': Reader Store.ModuleTable ': NameIdGen ': r) a ->
  Sem r (PipelineResult a)
processFileUpTo a = do
  entry <- ask
  res <- processFileUpToParsing entry
  let pkg = entry ^. entryPointPackage
  mid <- runReader pkg (getModuleId (res ^. pipelineResult . Parser.resultModule . modulePath . to topModulePathKey))
  a' <-
    evalTopNameIdGen mid
      . runReader (res ^. pipelineResultImports)
      . runReader (res ^. pipelineResult)
      $ a
  return (set pipelineResult a' res)

processImports ::
  forall r.
  (Members '[Reader EntryPoint, ModuleInfoCache, Error JuvixError, Files, PathResolver] r) =>
  [TopModulePath] ->
  Sem r CompileResult
processImports imports = do
  ms :: [PipelineResult Store.ModuleInfo] <- forM imports processImport
  let mtab =
        Store.mkModuleTable (map (^. pipelineResult) ms)
          <> mconcatMap (^. pipelineResultImports) ms
      changed = any (^. pipelineResultChanged) ms
  return
    CompileResult
      { _compileResultChanged = changed,
        _compileResultModuleTable = mtab
      }

processModuleToStoredCore ::
  forall r.
  (Members '[ModuleInfoCache, PathResolver, TopModuleNameChecker, Error JuvixError, Files] r) =>
  Text ->
  EntryPoint ->
  Sem r (PipelineResult Store.ModuleInfo)
processModuleToStoredCore sha256 entry = over pipelineResult mkModuleInfo <$> processFileToStoredCore entry
  where
    mkModuleInfo :: Core.CoreResult -> Store.ModuleInfo
    mkModuleInfo Core.CoreResult {..} =
      Store.ModuleInfo
        { _moduleInfoScopedModule = scoperResult ^. Scoper.resultScopedModule,
          _moduleInfoInternalModule = _coreResultInternalTypedResult ^. InternalTyped.resultInternalModule,
          _moduleInfoCoreTable = fromCore (_coreResultModule ^. Core.moduleInfoTable),
          _moduleInfoImports = map (^. importModulePath) $ scoperResult ^. Scoper.resultParserResult . Parser.resultParserState . parserStateImports,
          _moduleInfoOptions = StoredOptions.fromEntryPoint entry,
          _moduleInfoSHA256 = sha256,
          _moduleInfoFieldSize = entry ^. entryPointFieldSize
        }
      where
        scoperResult = _coreResultInternalTypedResult ^. InternalTyped.resultInternal . Internal.resultScoper

processFileToStoredCore ::
  forall r.
  (Members '[ModuleInfoCache, PathResolver, TopModuleNameChecker, Error JuvixError, Files] r) =>
  EntryPoint ->
  Sem r (PipelineResult Core.CoreResult)
processFileToStoredCore entry = ignoreHighlightBuilder . runReader entry $ do
  res <- processFileUpToParsing entry
  let pkg = entry ^. entryPointPackage
  mid <- runReader pkg (getModuleId (res ^. pipelineResult . Parser.resultModule . modulePath . to topModulePathKey))
  r <-
    evalTopNameIdGen mid
      . runReader (res ^. pipelineResultImports)
      . runReader (res ^. pipelineResult)
      $ upToStoredCore
  return (set pipelineResult r res)
