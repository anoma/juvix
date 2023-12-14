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

processFile ::
  forall r.
  (Members '[Error JuvixError, Files, GitClone, PathResolver] r) =>
  EntryPoint ->
  Sem r (Parser.ParserResult, Store.ModuleTable)
processFile entry = runReader @ImportParents mempty $ processFile' entry

processImport ::
  forall r.
  (Members '[Error JuvixError, Files, GitClone, PathResolver] r) =>
  EntryPoint ->
  Import 'Parsed ->
  Sem r (Store.ModuleInfo, Store.ModuleTable)
processImport entry i = runReader @ImportParents mempty $ processImport' entry (i ^. importModulePath)

processModule ::
  forall r.
  (Members '[Error JuvixError, Files, GitClone, PathResolver] r) =>
  EntryPoint ->
  Sem r (Store.ModuleInfo, Store.ModuleTable)
processModule entry = runReader @ImportParents mempty $ processModule' entry

processFileToStoredCore ::
  forall r.
  (Members '[Error JuvixError, Files, GitClone, PathResolver] r) =>
  EntryPoint ->
  Sem r (Core.CoreResult, Store.ModuleTable)
processFileToStoredCore entry = runReader @ImportParents mempty $ processFileToStoredCore' entry

processFile' ::
  forall r.
  (Members '[Reader ImportParents, Error JuvixError, Files, GitClone, PathResolver] r) =>
  EntryPoint ->
  Sem r (Parser.ParserResult, Store.ModuleTable)
processFile' entry = do
  res <- runReader entry upToParsing
  let imports = res ^. Parser.resultParserState . Parser.parserStateImports
  mtab <- processImports' entry (map (^. importModulePath) imports)
  return (res, mtab)

processImports' ::
  forall r.
  (Members '[Reader ImportParents, Error JuvixError, Files, GitClone, PathResolver] r) =>
  EntryPoint ->
  [TopModulePath] ->
  Sem r Store.ModuleTable
processImports' entry imports = do
  ms <- forM imports (processImport' entry)
  return $ Store.mkModuleTable (map fst ms) <> mconcatMap snd ms

processImport' ::
  forall r.
  (Members '[Reader ImportParents, Error JuvixError, Files, GitClone, PathResolver] r) =>
  EntryPoint ->
  TopModulePath ->
  Sem r (Store.ModuleInfo, Store.ModuleTable)
processImport' entry p = do
  checkCycle
  local (over importParents (p :)) $
    withPath'
      p
      ( \path ->
          processModule' (entry {_entryPointModulePath = Just path})
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
  (Members '[Reader ImportParents, Error JuvixError, Files, GitClone, PathResolver] r) =>
  EntryPoint ->
  Sem r (Core.CoreResult, Store.ModuleTable)
processFileToStoredCore' entry = do
  (res, mtab) <- processFile' entry
  fmap (,mtab)
    $ evalTopNameIdGen
      (res ^. Parser.resultModule . moduleId)
    $ runReader mtab
    $ runReader entry
    $ runReader res upToStoredCore

processModule' ::
  forall r.
  (Members '[Reader ImportParents, Error JuvixError, Files, GitClone, PathResolver] r) =>
  EntryPoint ->
  Sem r (Store.ModuleInfo, Store.ModuleTable)
processModule' entry = do
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
          return (info, mtab)
    _ -> do
      (info, mtab) <- processModule'' sha256 entry
      saveToFile absPath info
      return (info, mtab)
  where
    root = entry ^. entryPointRoot
    sourcePath = fromJust $ entry ^. entryPointModulePath
    opts = StoredModule.fromEntryPoint entry

processModule'' ::
  forall r.
  (Members '[Reader ImportParents, Error JuvixError, Files, GitClone, PathResolver] r) =>
  Text ->
  EntryPoint ->
  Sem r (Store.ModuleInfo, Store.ModuleTable)
processModule'' sha256 entry = first mkModuleInfo <$> processFileToStoredCore' entry
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

processFileUpTo ::
  forall r a.
  (Members '[Reader EntryPoint, Error JuvixError, Files, GitClone, PathResolver] r) =>
  Sem (Reader Parser.ParserResult ': Reader Store.ModuleTable ': NameIdGen ': r) a ->
  Sem r (a, Store.ModuleTable)
processFileUpTo a = do
  entry <- ask
  (res, mtab) <- processFile entry
  fmap (,mtab)
    $ evalTopNameIdGen
      (res ^. Parser.resultModule . moduleId)
    $ runReader mtab
    $ runReader res a

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
