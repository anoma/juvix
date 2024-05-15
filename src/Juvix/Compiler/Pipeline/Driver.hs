module Juvix.Compiler.Pipeline.Driver
  ( processFileUpTo,
    processFileToStoredCore,
    processImport,
    processRecursiveUpToTyped,
    ModuleInfoCache,
    evalModuleInfoCache,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Concrete (ImportCycle (ImportCycle), ScoperError (ErrImportCycle))
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
import Juvix.Compiler.Pipeline.ImportParents
import Juvix.Compiler.Pipeline.Loader.PathResolver
import Juvix.Compiler.Pipeline.ModuleInfoCache
import Juvix.Compiler.Store.Core.Extra
import Juvix.Compiler.Store.Extra qualified as Store
import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Compiler.Store.Options qualified as StoredModule
import Juvix.Compiler.Store.Options qualified as StoredOptions
import Juvix.Data.CodeAnn
import Juvix.Data.Effect.TaggedLock
import Juvix.Data.SHA256 qualified as SHA256
import Juvix.Extra.Serialize
import Juvix.Prelude
import Path.Posix qualified as Path

data CompileResult = CompileResult
  { _compileResultModuleTable :: Store.ModuleTable,
    _compileResultChanged :: Bool
  }

makeLenses ''CompileResult

instance Semigroup CompileResult where
  sconcat l =
    CompileResult
      { _compileResultChanged = any (^. compileResultChanged) l,
        _compileResultModuleTable = sconcatMap (^. compileResultModuleTable) l
      }

instance Monoid CompileResult where
  mempty =
    CompileResult
      { _compileResultChanged = False,
        _compileResultModuleTable = mempty
      }

evalModuleInfoCache ::
  forall r a.
  (Members '[TaggedLock, TopModuleNameChecker, Error JuvixError, Files, PathResolver] r) =>
  Sem (ModuleInfoCache ': Reader ImportParents ': r) a ->
  Sem r a
evalModuleInfoCache = runReader @ImportParents mempty . evalCacheEmpty processModule

processFileUpToParsing ::
  forall r.
  (Members '[TaggedLock, TopModuleNameChecker, HighlightBuilder, Error JuvixError, Files, PathResolver, ModuleInfoCache] r) =>
  EntryPoint ->
  Sem r (PipelineResult Parser.ParserResult)
processFileUpToParsing entry =
  runReader @ImportParents mempty $
    processFileUpToParsing' entry

processImport ::
  forall r.
  (Members '[TaggedLock, Error JuvixError, Files, PathResolver, ModuleInfoCache] r) =>
  EntryPoint ->
  Import 'Parsed ->
  Sem r (PipelineResult Store.ModuleInfo)
processImport entry i =
  runReader @ImportParents mempty $
    processImport' entry (i ^. importModulePath)

processFileToStoredCore ::
  forall r.
  (Members '[TaggedLock, TopModuleNameChecker, Error JuvixError, Files, PathResolver, ModuleInfoCache] r) =>
  EntryPoint ->
  Sem r (PipelineResult Core.CoreResult)
processFileToStoredCore entry =
  runReader @ImportParents mempty $
    processFileToStoredCore' entry

processFileUpTo ::
  forall r a.
  (Members '[TaggedLock, TopModuleNameChecker, HighlightBuilder, Reader EntryPoint, Error JuvixError, Files, PathResolver, ModuleInfoCache] r) =>
  Sem (Reader Parser.ParserResult ': Reader Store.ModuleTable ': NameIdGen ': r) a ->
  Sem r (PipelineResult a)
processFileUpTo a = do
  entry <- ask
  res <- processFileUpToParsing entry
  mid <- getModuleId (res ^. pipelineResult . Parser.resultModule . modulePath)
  a' <-
    evalTopNameIdGen mid
      . runReader (res ^. pipelineResultImports)
      . runReader (res ^. pipelineResult)
      $ a
  return (set pipelineResult a' res)

processFileUpToParsing' ::
  forall r.
  (Members '[HighlightBuilder, TopModuleNameChecker, Reader ImportParents, Error JuvixError, Files, PathResolver, ModuleInfoCache] r) =>
  EntryPoint ->
  Sem r (PipelineResult Parser.ParserResult)
processFileUpToParsing' entry = do
  res <- runReader entry upToParsing
  let imports :: [Import 'Parsed] = res ^. Parser.resultParserState . Parser.parserStateImports
  mtab <- (^. compileResultModuleTable) <$> processImports entry (map (^. importModulePath) imports)
  return
    PipelineResult
      { _pipelineResult = res,
        _pipelineResultImports = mtab,
        _pipelineResultChanged = True
      }

processImports ::
  forall r.
  (Members '[Reader ImportParents, Error JuvixError, Files, PathResolver, ModuleInfoCache] r) =>
  EntryPoint ->
  [TopModulePath] ->
  Sem r CompileResult
processImports entry imports = do
  ms :: [PipelineResult Store.ModuleInfo] <- forM imports (processImport' entry)
  let mtab =
        Store.mkModuleTable (map (^. pipelineResult) ms)
          <> mconcatMap (^. pipelineResultImports) ms
      changed = any (^. pipelineResultChanged) ms
  return
    CompileResult
      { _compileResultChanged = changed,
        _compileResultModuleTable = mtab
      }

processImport' ::
  forall r a.
  (Members '[Reader ImportParents, Error JuvixError, Files, PathResolver, ModuleInfoCache' a] r) =>
  EntryPoint ->
  TopModulePath ->
  Sem r a
processImport' entry p = do
  checkCycle
  local (over importParents (p :)) $
    withPathFile p getCachedImport
  where
    checkCycle :: Sem r ()
    checkCycle = do
      topp <- asks (^. importParents)
      case span (/= p) topp of
        (_, []) -> return ()
        (c, _) ->
          let cyc = NonEmpty.reverse (p :| c)
           in mapError (JuvixError @ScoperError) $
                throw (ErrImportCycle (ImportCycle cyc))

    getCachedImport :: Path Abs File -> Sem r a
    getCachedImport path = cacheGet (EntryIndex entry')
      where
        entry' =
          entry
            { _entryPointStdin = Nothing,
              _entryPointModulePath = Just path
            }

processFileToStoredCore' ::
  forall r.
  (Members '[Reader ImportParents, TopModuleNameChecker, Error JuvixError, Files, PathResolver, ModuleInfoCache] r) =>
  EntryPoint ->
  Sem r (PipelineResult Core.CoreResult)
processFileToStoredCore' entry = ignoreHighlightBuilder . runReader entry $ do
  res <- processFileUpToParsing' entry
  mid <- getModuleId (res ^. pipelineResult . Parser.resultModule . modulePath)
  r <-
    evalTopNameIdGen mid
      . runReader (res ^. pipelineResultImports)
      . runReader (res ^. pipelineResult)
      $ upToStoredCore
  return (set pipelineResult r res)

processModule ::
  forall r.
  (Members '[TaggedLock, TopModuleNameChecker, Reader ImportParents, Error JuvixError, Files, PathResolver, ModuleInfoCache] r) =>
  EntryIndex ->
  Sem r (PipelineResult Store.ModuleInfo)
processModule (EntryIndex entry) = do
  let buildDir = resolveAbsBuildDir root (entry ^. entryPointBuildDir)
      sourcePath = fromJust (entry ^. entryPointModulePath)
      relPath =
        fromJust
          . replaceExtension ".jvo"
          . fromJust
          $ stripProperPrefix $(mkAbsDir "/") sourcePath
      absPath = buildDir Path.</> relPath
  -- traceM $
  --   "File = "
  --     <> pack (toFilePath sourcePath)
  --     <> "\n"
  --     <> "Root = "
  --     <> pack (toFilePath (entry ^. entryPointRoot))
  -- traceM ("Node = " <> show (entryPointNode entry))
  sha256 <- SHA256.digestFile sourcePath
  m :: Maybe Store.ModuleInfo <- loadFromFile absPath
  case m of
    Just info
      | info ^. Store.moduleInfoSHA256 == sha256
          && info ^. Store.moduleInfoOptions == opts
          && info ^. Store.moduleInfoFieldSize == entry ^. entryPointFieldSize -> do
          CompileResult {..} <- processImports entry (info ^. Store.moduleInfoImports)
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
    root = entry ^. entryPointRoot
    opts = StoredModule.fromEntryPoint entry

    recompile :: Text -> Path Abs File -> Sem r (PipelineResult Store.ModuleInfo)
    recompile sha256 absPath = do
      res <- processModuleToStoredCore sha256 entry
      saveToFile absPath (res ^. pipelineResult)
      return res

entryPointNode :: EntryPoint -> ImportNode
entryPointNode e =
  ImportNode
    { _importNodePackageRoot = root,
      _importNodeFile = fromMaybe err (stripProperPrefix root srcFile)
    }
  where
    err :: a
    err =
      error $
        "unexpected: expected the path of the input file to have the root as prefix\n"
          <> "root = "
          <> show root
          <> "\n"
          <> "srcFile = "
          <> show srcFile
    root = e ^. entryPointRoot
    srcFile = fromJust (e ^. entryPointModulePath)

processModuleToStoredCore ::
  forall r.
  (Members '[Reader ImportParents, TopModuleNameChecker, Error JuvixError, Files, PathResolver, ModuleInfoCache] r) =>
  Text ->
  EntryPoint ->
  Sem r (PipelineResult Store.ModuleInfo)
processModuleToStoredCore sha256 entry = over pipelineResult mkModuleInfo <$> processFileToStoredCore' entry
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

processRecursiveUpToTyped ::
  forall r.
  (Members '[Reader EntryPoint, TopModuleNameChecker, TaggedLock, HighlightBuilder, Error JuvixError, Files, PathResolver, ModuleInfoCache] r) =>
  Sem r (InternalTypedResult, [InternalTypedResult])
processRecursiveUpToTyped = do
  entry <- ask
  PipelineResult res mtab _ <- processFileUpToParsing entry
  let imports = HashMap.keys (mtab ^. Store.moduleTable)
  ms <- forM imports (`withPathFile` goImport)
  mid <- getModuleId (res ^. Parser.resultModule . modulePath)
  a <-
    evalTopNameIdGen mid
      . runReader mtab
      . runReader res
      $ upToInternalTyped
  return (a, ms)
  where
    goImport :: Path Abs File -> Sem r InternalTypedResult
    goImport path = do
      entry <- ask
      let entry' =
            entry
              { _entryPointStdin = Nothing,
                _entryPointModulePath = Just path
              }
      (^. pipelineResult) <$> runReader entry' (processFileUpTo upToInternalTyped)
