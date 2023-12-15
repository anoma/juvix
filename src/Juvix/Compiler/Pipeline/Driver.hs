module Juvix.Compiler.Pipeline.Driver
  ( processFile,
    processFileUpTo,
    processFileToStoredCore,
    processModule,
    processImport,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Concrete (ImportCycle (ImportCycle), ScoperError (ErrImportCycle))
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Concrete.Translation.FromSource.Data.ParserState (parserStateImports)
import Juvix.Compiler.Concrete.Translation.FromSource.Data.ParserState qualified as Parser
import Juvix.Compiler.Core.Data.Module qualified as Core
import Juvix.Compiler.Core.Translation.FromInternal.Data.Context qualified as Core
import Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as InternalTyped
import Juvix.Compiler.Pipeline
import Juvix.Compiler.Pipeline.Loader.PathResolver
import Juvix.Compiler.Store.Core.Extra
import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Compiler.Store.Options qualified as StoredModule
import Juvix.Compiler.Store.Options qualified as StoredOptions
import Juvix.Data.CodeAnn
import Juvix.Data.Effect.Git
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

type MCache = Cache EntryIndex (PipelineResult Store.ModuleInfo)

processFile ::
  forall r.
  (Members '[Error JuvixError, Files, GitClone, PathResolver] r) =>
  EntryPoint ->
  Sem r (PipelineResult Parser.ParserResult)
processFile entry =
  runReader @ImportParents mempty $
    evalCacheEmpty processModule' $
      processFile' entry

processImport ::
  forall r.
  (Members '[Error JuvixError, Files, GitClone, PathResolver] r) =>
  EntryPoint ->
  Import 'Parsed ->
  Sem r (PipelineResult Store.ModuleInfo)
processImport entry i =
  runReader @ImportParents mempty $
    evalCacheEmpty processModule' $
      processImport' entry (i ^. importModulePath)

processModule ::
  forall r.
  (Members '[Error JuvixError, Files, GitClone, PathResolver] r) =>
  EntryPoint ->
  Sem r (PipelineResult Store.ModuleInfo)
processModule entry =
  runReader @ImportParents mempty $
    evalCacheEmpty processModule' $
      processModule' (EntryIndex entry)

processFileToStoredCore ::
  forall r.
  (Members '[Error JuvixError, Files, GitClone, PathResolver] r) =>
  EntryPoint ->
  Sem r (PipelineResult Core.CoreResult)
processFileToStoredCore entry =
  runReader @ImportParents mempty $
    evalCacheEmpty processModule' $
      processFileToStoredCore' entry

processFileUpTo ::
  forall r a.
  (Members '[Reader EntryPoint, Error JuvixError, Files, GitClone, PathResolver] r) =>
  Sem (Reader Parser.ParserResult ': Reader Store.ModuleTable ': NameIdGen ': r) a ->
  Sem r (PipelineResult a)
processFileUpTo a = do
  entry <- ask
  res <- processFile entry
  a' <-
    evalTopNameIdGen
      (res ^. pipelineResult . Parser.resultModule . moduleId)
      $ runReader (res ^. pipelineResultImports)
      $ runReader (res ^. pipelineResult) a
  return $ set pipelineResult a' res

processFile' ::
  forall r.
  (Members '[Reader ImportParents, Error JuvixError, Files, GitClone, PathResolver, MCache] r) =>
  EntryPoint ->
  Sem r (PipelineResult Parser.ParserResult)
processFile' entry = do
  res <- runReader entry upToParsing
  let imports = res ^. Parser.resultParserState . Parser.parserStateImports
  mtab <- processImports' entry (map (^. importModulePath) imports)
  return (PipelineResult res mtab)

processImports' ::
  forall r.
  (Members '[Reader ImportParents, Error JuvixError, Files, GitClone, PathResolver, MCache] r) =>
  EntryPoint ->
  [TopModulePath] ->
  Sem r Store.ModuleTable
processImports' entry imports = do
  ms <- forM imports (processImport' entry)
  return $ Store.mkModuleTable (map (^. pipelineResult) ms) <> mconcatMap (^. pipelineResultImports) ms

processImport' ::
  forall r.
  (Members '[Reader ImportParents, Error JuvixError, Files, GitClone, PathResolver, MCache] r) =>
  EntryPoint ->
  TopModulePath ->
  Sem r (PipelineResult Store.ModuleInfo)
processImport' entry p = do
  checkCycle
  local (over importParents (p :)) $
    withPath'
      p
      ( \path ->
          cacheGet (EntryIndex entry {_entryPointModulePath = Just path})
      )
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

processFileToStoredCore' ::
  forall r.
  (Members '[Reader ImportParents, Error JuvixError, Files, GitClone, PathResolver, MCache] r) =>
  EntryPoint ->
  Sem r (PipelineResult Core.CoreResult)
processFileToStoredCore' entry = do
  res <- processFile' entry
  r <-
    evalTopNameIdGen
      (res ^. pipelineResult . Parser.resultModule . moduleId)
      $ runReader (res ^. pipelineResultImports)
      $ runReader entry
      $ runReader (res ^. pipelineResult) upToStoredCore
  return $ set pipelineResult r res

processModule' ::
  forall r.
  (Members '[Reader ImportParents, Error JuvixError, Files, GitClone, PathResolver, MCache] r) =>
  EntryIndex ->
  Sem r (PipelineResult Store.ModuleInfo)
processModule' (EntryIndex entry) = do
  let buildDir = resolveAbsBuildDir root (entry ^. entryPointBuildDir)
      relPath = fromJust $ replaceExtension ".jvo" $ fromJust $ stripProperPrefix $(mkAbsDir "/") sourcePath
      absPath = buildDir Path.</> relPath
  sha256 <- SHA256.digestFile sourcePath
  m :: Maybe Store.ModuleInfo <- loadFromFile absPath
  case m of
    Just info
      | info ^. Store.moduleInfoSHA256 == sha256
          && info ^. Store.moduleInfoOptions == opts -> do
          mtab <- processImports' entry (info ^. Store.moduleInfoImports)
          return (PipelineResult info mtab)
    _ -> do
      res <- processModule'' sha256 entry
      saveToFile absPath (res ^. pipelineResult)
      return res
  where
    root = entry ^. entryPointRoot
    sourcePath = fromJust $ entry ^. entryPointModulePath
    opts = StoredModule.fromEntryPoint entry

processModule'' ::
  forall r.
  (Members '[Reader ImportParents, Error JuvixError, Files, GitClone, PathResolver, MCache] r) =>
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
          _moduleInfoSHA256 = sha256
        }
      where
        scoperResult = _coreResultInternalTypedResult ^. InternalTyped.resultInternal . Internal.resultScoper

withPath' ::
  forall r a.
  (Members '[PathResolver, Error JuvixError] r) =>
  TopModulePath ->
  (Path Abs File -> Sem r a) ->
  Sem r a
withPath' path a = withPathFile path (either throwError a)
  where
    throwError :: PathResolverError -> Sem r a
    throwError e =
      mapError (JuvixError @GenericError) $
        throw
          GenericError
            { _genericErrorLoc = loc,
              _genericErrorMessage = mkAnsiText $ ppCodeAnn e,
              _genericErrorIntervals = [loc]
            }
      where
        loc = getLoc path
