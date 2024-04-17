module Juvix.Compiler.Pipeline.Driver
  ( processFileUpTo,
    processFileToStoredCore,
    processImport,
    processRecursiveUpToTyped,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Concrete (ImportCycle (ImportCycle), ScoperError (ErrImportCycle))
import Juvix.Compiler.Concrete.Data.Highlight
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Concrete.Translation.FromSource.Data.ParserState (parserStateImports)
import Juvix.Compiler.Concrete.Translation.FromSource.Data.ParserState qualified as Parser
import Juvix.Compiler.Core.Data.Module qualified as Core
import Juvix.Compiler.Core.Translation.FromInternal.Data.Context qualified as Core
import Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as InternalTyped
import Juvix.Compiler.Internal.Translation.FromInternal.Data (InternalTypedResult)
import Juvix.Compiler.Pipeline
import Juvix.Compiler.Pipeline.Loader.PathResolver
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

newtype ImportParents = ImportParents
  { _importParents :: [TopModulePath]
  }
  deriving newtype (Semigroup, Monoid)

makeLenses ''ImportParents

newtype EntryIndex = EntryIndex
  { _entryIxEntry :: EntryPoint
  }

makeLenses ''EntryIndex

instance Eq EntryIndex where
  (==) = (==) `on` (^. entryIxEntry . entryPointModulePath)

instance Hashable EntryIndex where
  hashWithSalt s = hashWithSalt s . (^. entryIxEntry . entryPointModulePath)

type MCache' a = Cache EntryIndex a

type MCache = MCache' (PipelineResult Store.ModuleInfo)

processFileUpToParsing ::
  forall r.
  (Members '[TaggedLock, HighlightBuilder, Error JuvixError, Files, PathResolver] r) =>
  EntryPoint ->
  Sem r (PipelineResult Parser.ParserResult)
processFileUpToParsing entry =
  runReader @ImportParents mempty
    . evalCacheEmpty processModule'
    $ processFileUpToParsing' entry

processImport ::
  forall r.
  (Members '[TaggedLock, Error JuvixError, Files, PathResolver] r) =>
  EntryPoint ->
  Import 'Parsed ->
  Sem r (PipelineResult Store.ModuleInfo)
processImport entry i =
  runReader @ImportParents mempty
    . evalCacheEmpty processModule'
    $ processImport' entry (i ^. importModulePath)

-- TODO parallelize
processFileToStoredCore ::
  forall r.
  (Members '[TaggedLock, Error JuvixError, Files, PathResolver] r) =>
  EntryPoint ->
  Sem r (PipelineResult Core.CoreResult)
processFileToStoredCore entry =
  runReader @ImportParents mempty
    . evalCacheEmpty processModule'
    $ processFileToStoredCore' entry

processFileUpTo ::
  forall r a.
  (Members '[TaggedLock, HighlightBuilder, Reader EntryPoint, Error JuvixError, Files, PathResolver] r) =>
  Sem (Reader Parser.ParserResult ': Reader Store.ModuleTable ': NameIdGen ': r) a ->
  Sem r (PipelineResult a)
processFileUpTo a = do
  entry <- ask
  res <- processFileUpToParsing entry
  a' <-
    evalTopNameIdGen
      (res ^. pipelineResult . Parser.resultModule . moduleId)
      . runReader (res ^. pipelineResultImports)
      . runReader (res ^. pipelineResult)
      $ a
  return (set pipelineResult a' res)

processFileUpToParsing' ::
  forall r.
  (Members '[HighlightBuilder, Reader ImportParents, Error JuvixError, Files, PathResolver, MCache] r) =>
  EntryPoint ->
  Sem r (PipelineResult Parser.ParserResult)
processFileUpToParsing' entry = do
  res <- runReader entry upToParsing
  let imports = res ^. Parser.resultParserState . Parser.parserStateImports
  mtab <- processImports' entry (map (^. importModulePath) imports)
  return
    PipelineResult
      { _pipelineResult = res,
        _pipelineResultImports = mtab,
        _pipelineResultChanged = True
      }

processImports' ::
  forall r.
  (Members '[Reader ImportParents, Error JuvixError, Files, PathResolver, MCache] r) =>
  EntryPoint ->
  [TopModulePath] ->
  Sem r Store.ModuleTable
processImports' entry imports = snd <$> processImports'' entry imports

processImports'' ::
  forall r.
  (Members '[Reader ImportParents, Error JuvixError, Files, PathResolver, MCache] r) =>
  EntryPoint ->
  [TopModulePath] ->
  Sem r (Bool, Store.ModuleTable)
processImports'' entry imports = do
  ms <- forM imports (processImport' entry)
  let mtab =
        Store.mkModuleTable (map (^. pipelineResult) ms)
          <> mconcatMap (^. pipelineResultImports) ms
      changed = any (^. pipelineResultChanged) ms
  return (changed, mtab)

processImport' ::
  forall r a.
  (Members '[Reader ImportParents, Error JuvixError, Files, PathResolver, MCache' a] r) =>
  EntryPoint ->
  TopModulePath ->
  Sem r a
processImport' entry p = do
  checkCycle
  local (over importParents (p :)) $
    withPath' p getCachedImport
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
  (Members '[Reader ImportParents, Error JuvixError, Files, PathResolver, MCache] r) =>
  EntryPoint ->
  Sem r (PipelineResult Core.CoreResult)
processFileToStoredCore' entry = ignoreHighlightBuilder $ do
  res <- processFileUpToParsing' entry
  r <-
    evalTopNameIdGen (res ^. pipelineResult . Parser.resultModule . moduleId)
      . runReader (res ^. pipelineResultImports)
      . runReader entry
      . runReader (res ^. pipelineResult)
      $ upToStoredCore
  return (set pipelineResult r res)

processModule' ::
  forall r.
  (Members '[TaggedLock, Reader ImportParents, Error JuvixError, Files, PathResolver, MCache] r) =>
  EntryIndex ->
  Sem r (PipelineResult Store.ModuleInfo)
processModule' (EntryIndex entry) = do
  let buildDir = resolveAbsBuildDir root (entry ^. entryPointBuildDir)
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
          (changed, mtab) <- processImports'' entry (info ^. Store.moduleInfoImports)
          -- We need to check whether any of the recursive imports is fragile,
          -- not only the direct ones, because identifiers may be re-exported
          -- (with `open public`).
          let fragile = any (^. Store.moduleInfoFragile) (mtab ^. Store.moduleTable)
          if
              | changed && fragile ->
                  recompile sha256 absPath
              | otherwise ->
                  return
                    PipelineResult
                      { _pipelineResult = info,
                        _pipelineResultImports = mtab,
                        _pipelineResultChanged = False
                      }
    _ ->
      recompile sha256 absPath
  where
    root = entry ^. entryPointRoot
    sourcePath = fromJust $ entry ^. entryPointModulePath
    opts = StoredModule.fromEntryPoint entry

    recompile :: Text -> Path Abs File -> Sem r (PipelineResult Store.ModuleInfo)
    recompile sha256 absPath = do
      res <- processModule'' sha256 entry
      saveToFile absPath (res ^. pipelineResult)
      return res

processModule'' ::
  forall r.
  (Members '[Reader ImportParents, Error JuvixError, Files, PathResolver, MCache] r) =>
  Text ->
  EntryPoint ->
  Sem r (PipelineResult Store.ModuleInfo)
processModule'' sha256 entry = over pipelineResult mkModuleInfo <$> processFileToStoredCore' entry
  where
    mkModuleInfo :: Core.CoreResult -> Store.ModuleInfo
    mkModuleInfo Core.CoreResult {..} =
      Store.ModuleInfo
        { _moduleInfoScopedModule = scoperResult ^. Scoper.resultScopedModule,
          _moduleInfoInternalModule = _coreResultInternalTypedResult ^. InternalTyped.resultInternalModule,
          _moduleInfoCoreTable = fromCore (_coreResultModule ^. Core.moduleInfoTable),
          _moduleInfoImports = map (^. importModulePath) $ scoperResult ^. Scoper.resultParserResult . Parser.resultParserState . parserStateImports,
          _moduleInfoOptions = StoredOptions.fromEntryPoint entry,
          _moduleInfoFragile = Core.moduleIsFragile _coreResultModule,
          _moduleInfoSHA256 = sha256,
          _moduleInfoFieldSize = entry ^. entryPointFieldSize
        }
      where
        scoperResult = _coreResultInternalTypedResult ^. InternalTyped.resultInternal . Internal.resultScoper

processRecursiveUpToTyped ::
  forall r.
  (Members '[Reader EntryPoint, TaggedLock, HighlightBuilder, Error JuvixError, Files, PathResolver] r) =>
  Sem r (InternalTypedResult, [InternalTypedResult])
processRecursiveUpToTyped = do
  entry <- ask
  PipelineResult res mtab _ <- processFileUpToParsing entry
  let imports = HashMap.keys (mtab ^. Store.moduleTable)
  ms <- forM imports (`withPath'` goImport)
  a <-
    evalTopNameIdGen
      (res ^. Parser.resultModule . moduleId)
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

withPath' ::
  forall r a.
  (Members '[PathResolver, Error JuvixError] r) =>
  TopModulePath ->
  (Path Abs File -> Sem r a) ->
  Sem r a
withPath' path a = withPathFile path (either throwErr a)
  where
    throwErr :: PathResolverError -> Sem r a
    throwErr = mapError (JuvixError @PathResolverError) . throw
