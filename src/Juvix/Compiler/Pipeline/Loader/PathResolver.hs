module Juvix.Compiler.Pipeline.Loader.PathResolver
  ( module Juvix.Compiler.Pipeline.Loader.PathResolver.Paths,
    module Juvix.Compiler.Pipeline.Loader.PathResolver.Base,
    module Juvix.Compiler.Pipeline.Loader.PathResolver.Error,
    module Juvix.Compiler.Pipeline.Loader.PathResolver.Data,
    module Juvix.Compiler.Pipeline.Loader.PathResolver.PackageInfo,
    module Juvix.Compiler.Pipeline.Loader.PathResolver.DependencyResolver,
    runPathResolver,
    runPathResolverPipe,
    runPathResolverPipe',
    evalPathResolverPipe,
    findPackageJuvixFiles,
    mkImportTree,
    withImportTree,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Concrete.Data.Name
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error
import Juvix.Compiler.Concrete.Translation.ImportScanner
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Loader.PathResolver.Base
import Juvix.Compiler.Pipeline.Loader.PathResolver.Data
import Juvix.Compiler.Pipeline.Loader.PathResolver.DependencyResolver
import Juvix.Compiler.Pipeline.Loader.PathResolver.Error
import Juvix.Compiler.Pipeline.Loader.PathResolver.PackageInfo
import Juvix.Compiler.Pipeline.Loader.PathResolver.Paths
import Juvix.Compiler.Pipeline.Lockfile
import Juvix.Compiler.Pipeline.Package
import Juvix.Compiler.Pipeline.Package.Loader.EvalEff
import Juvix.Compiler.Pipeline.Root.Base (PackageType (..))
import Juvix.Data.Effect.TaggedLock
import Juvix.Data.SHA256 qualified as SHA256
import Juvix.Extra.Files
import Juvix.Extra.PackageFiles
import Juvix.Extra.Paths
import Juvix.Extra.Stdlib (ensureStdlib)
import Juvix.Prelude

mkPackage ::
  forall r.
  (Members '[Files, Error JuvixError, Reader ResolverEnv, DependencyResolver, EvalFileEff] r) =>
  Maybe EntryPoint ->
  Path Abs Dir ->
  Sem r Package
mkPackage mpackageEntry _packageRoot = do
  let buildDirDep = case mpackageEntry of
        Just packageEntry -> rootedBuildDir _packageRoot (packageEntry ^. entryPointBuildDir)
        Nothing -> DefaultBuildDir
  maybe (readPackage _packageRoot buildDirDep) (return . (^. entryPointPackage)) mpackageEntry

findPackageJuvixFiles :: (Members '[Files] r) => Path Abs Dir -> Sem r [Path Rel File]
findPackageJuvixFiles pkgRoot = map (fromJust . stripProperPrefix pkgRoot) <$> walkDirRelAccum juvixAccum pkgRoot []
  where
    juvixAccum :: Path Abs Dir -> [Path Rel Dir] -> [Path Rel File] -> [Path Abs File] -> Sem r ([Path Abs File], Recurse Rel)
    juvixAccum cd _ files acc = return (newJuvixFiles <> acc, RecurseFilter (\hasJuvixPackage d -> not hasJuvixPackage && not (isHiddenDirectory d)))
      where
        newJuvixFiles :: [Path Abs File]
        newJuvixFiles = [cd <//> f | f <- files, isJuvixOrJuvixMdFile f, not (isPackageFile f)]

mkPackageInfo ::
  forall r.
  (Members '[TaggedLock, Files, Error JuvixError, Error DependencyError, Reader ResolverEnv, DependencyResolver] r) =>
  Maybe EntryPoint ->
  Path Abs Dir ->
  Package ->
  Sem r PackageInfo
mkPackageInfo mpackageEntry _packageRoot pkg = do
  let buildDir :: Path Abs Dir = maybe (rootBuildDir _packageRoot) (someBaseToAbs _packageRoot . resolveBuildDir . (^. entryPointBuildDir)) mpackageEntry
  deps <- getDependencies
  let _packagePackage = PackageReal (set packageDependencies deps pkg)
  depsPaths <- mapM (fmap (^. resolvedDependencyPath) . resolveDependency . mkPackageDependencyInfo pkgFile) deps
  ensureStdlib _packageRoot buildDir deps
  files :: [Path Rel File] <- findPackageJuvixFiles _packageRoot
  globalPackageDescriptionAbsDir <- globalPackageDescriptionRoot
  globalPackageBaseAbsDir <- globalPackageBaseRoot
  let _packageJuvixRelativeFiles = keepJuvixFiles (hashSet files)
      _packageAvailableRoots =
        hashSet $
          globalPackageDescriptionAbsDir
            : globalPackageBaseAbsDir
            : _packageRoot
            : depsPaths
  return PackageInfo {..}
  where
    pkgFile :: Path Abs File
    pkgFile = pkg ^. packageFile

    -- Retrieve dependencies from:
    -- 1. The lockfile if it exists
    -- 2. The package file
    --
    -- If a lockfile is present this function throws an error if a package
    -- dependency is not specified in the lockfile
    getDependencies :: Sem r [Dependency]
    getDependencies = do
      mlockfile <- asks (^. envLockfileInfo)
      case mlockfile of
        Nothing -> return pkgDeps
        Just lf -> checkDeps lf
      where
        pkgDeps :: [Dependency]
        pkgDeps = pkg ^. packageDependencies

        checkDeps :: LockfileInfo -> Sem r [Dependency]
        checkDeps lf = mapM_ checkDep pkgDeps >> return lockfileDeps
          where
            lockfileDeps :: [Dependency]
            lockfileDeps = (^. lockfileDependencyDependency) <$> lf ^. lockfileInfoLockfile . lockfileDependencies

            lockfileDepNames :: HashSet Text
            lockfileDepNames = HashSet.fromList (mkName <$> lockfileDeps)

            mkName :: Dependency -> Text
            mkName = \case
              DependencyPath p -> pack (p ^. pathDependencyPath . prepath)
              DependencyGit g -> g ^. gitDependencyUrl

            checkDep :: Dependency -> Sem r ()
            checkDep d =
              unless
                (mkName d `HashSet.member` lockfileDepNames)
                ( throw
                    DependencyError
                      { _dependencyErrorPackageFile = pkgFile,
                        _dependencyErrorCause =
                          MissingLockfileDependencyError
                            MissingLockfileDependency
                              { _missingLockfileDependencyDependency = d,
                                _missingLockfileDependencyPath = lf ^. lockfileInfoPath
                              }
                      }
                )

lookupCachedDependency :: (Members '[State ResolverState, Reader ResolverEnv, Files, DependencyResolver] r) => Path Abs Dir -> Sem r (Maybe LockfileDependency)
lookupCachedDependency p = fmap (^. resolverCacheItemDependency) . HashMap.lookup p <$> gets (^. resolverCache)

registerPackageBase ::
  forall r.
  (Members '[Error ParserError, TaggedLock, State ResolverState, Files] r) =>
  Sem r ()
registerPackageBase = do
  packageBaseAbsDir <- globalPackageBaseRoot
  runReader packageBaseAbsDir updatePackageBaseFiles
  packageBaseRelFiles <- relFiles packageBaseAbsDir
  let pkgInfo =
        PackageInfo
          { _packageRoot = packageBaseAbsDir,
            _packageJuvixRelativeFiles = packageBaseRelFiles,
            _packagePackage = PackageBase,
            _packageAvailableRoots = HashSet.singleton packageBaseAbsDir
          }
      dep =
        LockfileDependency
          { _lockfileDependencyDependency = mkPathDependency (toFilePath packageBaseAbsDir),
            _lockfileDependencyDependencies = []
          }
      cacheItem =
        ResolverCacheItem
          { _resolverCacheItemPackage = pkgInfo,
            _resolverCacheItemDependency = dep
          }
  setResolverCacheItem packageBaseAbsDir (Just cacheItem)
  addPackageRelativeFiles pkgInfo

registerDependencies' ::
  forall r.
  (Members '[TaggedLock, Reader EntryPoint, Files, Error JuvixError, Error DependencyError, DependencyResolver, EvalFileEff] r) =>
  DependenciesConfig ->
  Sem (Reader ResolverEnv ': State ResolverState ': r) ()
registerDependencies' conf = do
  e <- ask @EntryPoint
  mapError (JuvixError @ParserError) registerPackageBase
  case e ^. entryPointPackageType of
    GlobalStdlib -> do
      glob <- globalRoot
      void (addRootDependency conf e glob)
    GlobalPackageBase -> return ()
    GlobalPackageDescription -> void (addRootDependency conf e (e ^. entryPointRoot))
    LocalPackage -> do
      lockfile <- addRootDependency conf e (e ^. entryPointRoot)
      whenM shouldWriteLockfile $ do
        packageFileChecksum <- SHA256.digestFile (e ^. entryPointPackage . packageFile)
        lockfilePath' <- lockfilePath
        writeLockfile lockfilePath' packageFileChecksum lockfile
  where
    shouldWriteLockfile :: Sem (Reader ResolverEnv ': State ResolverState ': r) Bool
    shouldWriteLockfile = do
      lockfileExists <- lockfilePath >>= fileExists'
      hasRemoteDependencies <- gets (^. resolverHasRemoteDependencies)
      shouldUpdateLockfile' <- gets (^. resolverShouldUpdateLockfile)

      let shouldForce = conf ^. dependenciesConfigForceUpdateLockfile
          shouldWriteInitialLockfile = not lockfileExists && hasRemoteDependencies
          shouldUpdateLockfile = lockfileExists && shouldUpdateLockfile'
      return (shouldForce || shouldWriteInitialLockfile || shouldUpdateLockfile)

    lockfilePath :: Sem (Reader ResolverEnv ': State ResolverState ': r) (Path Abs File)
    lockfilePath = do
      root <- asks (^. envRoot)
      return (mkPackageLockfilePath root)

addRootDependency ::
  forall r.
  (Members '[TaggedLock, State ResolverState, Reader ResolverEnv, Files, Error JuvixError, Error DependencyError, DependencyResolver, EvalFileEff] r) =>
  DependenciesConfig ->
  EntryPoint ->
  Path Abs Dir ->
  Sem r Lockfile
addRootDependency conf e root = do
  let pf = mkPackageFilePath root
      d = mkPackageDependencyInfo pf (mkPathDependency (toFilePath root))
  resolvedDependency <- resolveDependency d
  checkRemoteDependency resolvedDependency
  let p = resolvedDependency ^. resolvedDependencyPath
  withEnvInitialRoot p $ do
    pkg <- mkPackage (Just e) p
    shouldUpdateLockfile' <- shouldUpdateLockfile pkg
    when shouldUpdateLockfile' setShouldUpdateLockfile
    let resolvedPkg :: Package
          | shouldUpdateLockfile' = unsetPackageLockfile pkg
          | otherwise = pkg
    deps <- addDependency' resolvedPkg (Just e) resolvedDependency
    return Lockfile {_lockfileDependencies = deps ^. lockfileDependencyDependencies}
  where
    shouldUpdateLockfile :: Package -> Sem r Bool
    shouldUpdateLockfile pkg = do
      let checksumMay :: Maybe Text = pkg ^? packageLockfile . _Just . lockfileInfoChecksum . _Just
      packageFileChecksum <- SHA256.digestFile (pkg ^. packageFile)
      return (conf ^. dependenciesConfigForceUpdateLockfile || Just packageFileChecksum /= checksumMay)

addDependency ::
  forall r.
  (Members '[TaggedLock, State ResolverState, Reader ResolverEnv, Files, Error JuvixError, Error DependencyError, DependencyResolver, EvalFileEff] r) =>
  Maybe EntryPoint ->
  PackageDependencyInfo ->
  Sem r LockfileDependency
addDependency me d = do
  resolvedDependency <- resolveDependency d
  checkRemoteDependency resolvedDependency
  let p = resolvedDependency ^. resolvedDependencyPath
  cached <- lookupCachedDependency p
  case cached of
    Just cachedDep -> return cachedDep
    Nothing -> withEnvRoot p $ do
      pkg <- mkPackage me p
      addDependency' pkg me resolvedDependency

addPackageRelativeFiles :: (Member (State ResolverState) r) => PackageInfo -> Sem r ()
addPackageRelativeFiles pkgInfo =
  forM_ (pkgInfo ^. packageJuvixFiles) $ \f ->
    modify' (over resolverFiles (HashMap.insertWith (<>) f (pure pkgInfo)))

addDependency' ::
  forall r.
  (Members '[TaggedLock, State ResolverState, Reader ResolverEnv, Files, Error JuvixError, Error DependencyError, DependencyResolver, EvalFileEff] r) =>
  Package ->
  Maybe EntryPoint ->
  ResolvedDependency ->
  Sem r LockfileDependency
addDependency' pkg me resolvedDependency = do
  selectPackageLockfile pkg $ do
    pkgInfo <- mkPackageInfo me (resolvedDependency ^. resolvedDependencyPath) pkg
    addPackageRelativeFiles pkgInfo
    let packagePath = pkgInfo ^. packagePackage . packageLikeFile
    subDeps <-
      forM
        (pkgInfo ^. packagePackage . packageLikeDependencies)
        (\dep -> selectDependencyLockfile dep (addDependency Nothing (mkPackageDependencyInfo packagePath dep)))
    let dep =
          LockfileDependency
            { _lockfileDependencyDependency = resolvedDependency ^. resolvedDependencyDependency,
              _lockfileDependencyDependencies = subDeps
            }
        cacheItem =
          ResolverCacheItem
            { _resolverCacheItemPackage = pkgInfo,
              _resolverCacheItemDependency = dep
            }
    setResolverCacheItem (resolvedDependency ^. resolvedDependencyPath) (Just cacheItem)
    return dep
  where
    selectPackageLockfile :: Package -> Sem r a -> Sem r a
    selectPackageLockfile p action = do
      currentLockfile <- asks (^. envLockfileInfo)
      case currentLockfile of
        Just _ -> action
        Nothing -> case p ^. packageLockfile of
          Just lf -> withLockfile lf action
          Nothing -> action

    selectDependencyLockfile :: Dependency -> Sem r a -> Sem r a
    selectDependencyLockfile dep action = do
      currentLockfile <- asks (^. envLockfileInfo)
      case currentLockfile of
        Nothing -> action
        Just lf -> case extractLockfileInfo lf dep of
          Just dlf -> withLockfile dlf action
          Nothing -> action

currentPackage :: (Members '[Files, State ResolverState, Reader ResolverEnv] r) => Sem r PackageInfo
currentPackage = do
  curRoot <- asks (^. envRoot)
  (^. resolverCacheItemPackage) . fromJust <$> getResolverCacheItem curRoot

checkImportTreeCycles :: forall r. (Members '[Error ScoperError] r) => ImportTree -> Sem r ()
checkImportTreeCycles tree = do
  let sccs =
        stronglyConnComp
          [ (node, node, toList v) | (node, v) <- HashMap.toList (tree ^. importTree)
          ]
  whenJust (firstJust getCycle sccs) $ \(cyc :: NonEmpty ImportNode) ->
    throw
      . ErrImportCycleNew
      . ImportCycleNew
      $ getEdges cyc
  where
    getEdges :: NonEmpty ImportNode -> NonEmpty ImportScan
    getEdges = fmap (uncurry getEdge) . zipWithNextLoop

    getEdge :: ImportNode -> ImportNode -> ImportScan
    getEdge fromN toN = fromMaybe unexpected $ do
      edges <- tree ^. importTreeEdges . at fromN
      let rel :: Path Rel File = removeExtensions (toN ^. importNodeFile)
          cond :: ImportScan -> Bool
          cond = (== rel) . importScanToRelPath
      find cond edges
      where
        unexpected =
          error $
            "Impossible: Could not find edge between\n"
              <> prettyText fromN
              <> "\nand\n"
              <> prettyText toN
              <> "\n"
              <> "Available Edges:\n"
              <> prettyText (toList (tree ^. importTreeEdges . at fromN . _Just))

    getCycle :: SCC ImportNode -> Maybe (NonEmpty ImportNode)
    getCycle = \case
      AcyclicSCC {} -> Nothing
      CyclicSCC l -> Just (nonEmpty' l)

withImportTree ::
  forall r a.
  (Members '[Reader ImportScanStrategy, Error JuvixError, PathResolver, Files] r) =>
  Maybe (Path Abs File) ->
  Sem (Reader ImportTree ': r) a ->
  Sem r a
withImportTree entryModule x = do
  t <-
    mapError (JuvixError @ParserError)
      . mapError (JuvixError @ScoperError)
      $ mkImportTree entryModule
  runReader t x

-- | If an entry file is given, it scans imports reachable from that file,
-- otherwise it scans all files in all packages
mkImportTree ::
  forall r.
  (Members '[Reader ImportScanStrategy, Error ParserError, Error ScoperError, PathResolver, Files] r) =>
  Maybe (Path Abs File) ->
  Sem r ImportTree
mkImportTree mentrypointModulePath = do
  pkgInfosTable <- getPackageInfos
  let pkgs :: [PackageInfo] = toList pkgInfosTable
      allNodes :: [ImportNode] = concatMap packageNodes pkgs
      mEntryImportNode :: Maybe ImportNode
      mEntryImportNode = do
        absPath <- mentrypointModulePath
        let cond :: ImportNode -> Bool
            cond ImportNode {..} = absPath == _importNodePackageRoot <//> _importNodeFile
        find cond allNodes

      startingNodes = maybe allNodes pure mEntryImportNode
  tree <-
    execState (emptyImportTree allNodes)
      . runReader pkgInfosTable
      . evalVisitEmpty scanNode
      $ mapM_ visit startingNodes
  checkImportTreeCycles tree
  return tree
  where
    packageNodes :: PackageInfo -> [ImportNode]
    packageNodes pkg =
      [ ImportNode
          { _importNodePackageRoot = pkg ^. packageRoot,
            _importNodeFile = f
          }
        | f <- filter isJuvixOrJuvixMdFile (toList (pkg ^. packageJuvixRelativeFiles))
      ]

    addEdge :: forall r'. (Members '[State ImportTree] r') => ImportScan -> ImportNode -> ImportNode -> Sem r' ()
    addEdge importScan fromNode toNode = do
      modify (over importTree (insertHelper fromNode toNode))
      modify (over importTreeReverse (insertHelper toNode fromNode))
      modify (over importTreeEdges (insertHelper fromNode importScan))
      where
        insertHelper :: (Hashable k, Hashable v) => k -> v -> HashMap k (HashSet v) -> HashMap k (HashSet v)
        insertHelper k v = over (at k) (Just . maybe (HashSet.singleton v) (HashSet.insert v))

    getNodeImports ::
      forall r'.
      (Members '[Reader ImportScanStrategy, Files, Error ParserError] r') =>
      ImportNode ->
      Sem r' (HashSet ImportScan)
    getNodeImports n = (^. scanResultImports) <$> scanFileImports (n ^. importNodeAbsFile)

    scanNode ::
      forall r'.
      (Members '[State ImportTree, Reader (HashMap (Path Abs Dir) PackageInfo), Reader ImportScanStrategy, Error ParserError, Files, PathResolver, Visit ImportNode] r') =>
      ImportNode ->
      Sem r' ()
    scanNode fromNode = do
      scans <- toList <$> getNodeImports fromNode
      imports :: [ImportNode] <- mapM resolveImportScan scans
      forM_ (zipExact scans imports) $ \(importscan, toNode) -> do
        addEdge importscan fromNode toNode
        withResolverRoot (toNode ^. importNodePackageRoot) (visit toNode)

    resolveImportScan :: forall r'. (Members '[PathResolver] r') => ImportScan -> Sem r' ImportNode
    resolveImportScan s = do
      let rel = importScanToRelPath s
      (pkg, ext) <- resolvePath s
      return
        ImportNode
          { _importNodePackageRoot = pkg ^. packageRoot,
            _importNodeFile = addFileExt ext rel
          }

resolvePath' ::
  (Members '[Files, Error PathResolverError, State ResolverState, Reader ResolverEnv] r) =>
  ImportScan ->
  Sem r (PackageInfo, FileExt)
resolvePath' scan = do
  curPkg <- currentPackage
  filesToPackage <- gets (^. resolverFiles)
  let possibleExtensions = [FileExtJuvix, FileExtJuvixMarkdown]

      visible :: PackageInfo -> Bool
      visible pkg =
        HashSet.member (pkg ^. packageRoot) (curPkg ^. packageAvailableRoots)

      packagesWithExt :: [(PackageInfo, FileExt)]
      packagesWithExt =
        [ (pkg, ext)
          | ext <- possibleExtensions,
            let file = addFileExt ext (importScanToRelPath scan),
            pkgs <- toList (HashMap.lookup file filesToPackage),
            pkg <- toList pkgs,
            visible pkg
        ]
  case packagesWithExt of
    [(r, relPath)] -> return (r, relPath)
    [] ->
      throw $
        ErrMissingModule
          MissingModule
            { _missingInfo = curPkg,
              _missingModule = scan
            }
    (r, _) : rs ->
      throw $
        ErrDependencyConflict
          DependencyConflict
            { _conflictPackages = r :| map fst rs,
              _conflictPath = scan
            }

isModuleOrphan ::
  (Members '[Files] r) =>
  TopModulePath ->
  Sem r Bool
isModuleOrphan topJuvixPath = do
  let actualPath = getLoc topJuvixPath ^. intervalFile
      possiblePaths :: Path Abs Dir -> [Path Abs Dir]
      possiblePaths p = p : toList (parents p)
  packageFileExists <- findFile' (possiblePaths (parent actualPath)) packageFilePath
  yamlFileExists <- findFile' (possiblePaths (parent actualPath)) juvixYamlFile
  pathPackageDescription <- globalPackageDescriptionRoot
  pathPackageBase <- globalPackageBaseRoot
  return
    ( isNothing (packageFileExists <|> yamlFileExists)
        && not (pathPackageDescription `isProperPrefixOf` actualPath)
        && not (pathPackageBase `isProperPrefixOf` actualPath)
    )

expectedPath' ::
  (Members '[Reader ResolverEnv, Files] r) =>
  TopModulePath ->
  Sem r PathInfoTopModule
expectedPath' m = do
  let _pathInfoTopModule = m
  _rootInfoPath <- asks (^. envRoot)
  isOrphan <- isModuleOrphan m
  let _rootInfoKind
        | isOrphan = RootKindSingleFile
        | otherwise = RootKindPackage
      _pathInfoRootInfo = RootInfo {..}
  return PathInfoTopModule {..}

runPathResolver2 ::
  forall r a v.
  (v ~ '[TaggedLock, Reader EntryPoint, Files, Error JuvixError, Error DependencyError, DependencyResolver, EvalFileEff], Members v r) =>
  ResolverState ->
  ResolverEnv ->
  Sem (PathResolver ': r) a ->
  Sem r (ResolverState, a)
runPathResolver2 st topEnv arg = do
  ( reinterpretHCommon3
      ( mapError (JuvixError @PathResolverError)
          . runState st
          . runReader topEnv
      )
      handler
    )
    arg
  where
    handler ::
      forall t localEs x.
      (Members v t) =>
      LocalEnv localEs (Reader ResolverEnv ': State ResolverState ': Error PathResolverError ': t) ->
      PathResolver (Sem localEs) x ->
      Sem (Reader ResolverEnv ': State ResolverState ': Error PathResolverError ': t) x
    handler localEnv = \case
      RegisterDependencies forceUpdateLockfile -> registerDependencies' forceUpdateLockfile
      GetPackageInfos -> gets allPackageInfos
      ExpectedPathInfoTopModule m -> expectedPath' m
      ResolvePath relp -> resolvePath' relp
      WithResolverRoot root' m -> do
        e <- ask
        let _envSingleFile :: Maybe (Path Abs File)
            _envSingleFile
              | e ^. entryPointPackageType == GlobalStdlib = e ^. entryPointModulePath
              | otherwise = Nothing
            env' :: ResolverEnv -> ResolverEnv
            env' ResolverEnv {..} =
              ResolverEnv
                { _envRoot = root',
                  _envLockfileInfo = Nothing,
                  _envInitialRoot,
                  _envSingleFile
                }
        localSeqUnlift localEnv $ \unlift -> local env' (unlift m)

runPathResolver :: (Members '[TaggedLock, Reader EntryPoint, Files, Error JuvixError, Error DependencyError, DependencyResolver, EvalFileEff] r) => Path Abs Dir -> Sem (PathResolver ': r) a -> Sem r (ResolverState, a)
runPathResolver = runPathResolver' iniResolverState

runPathResolver' :: (Members '[TaggedLock, Reader EntryPoint, Files, Error JuvixError, Error DependencyError, DependencyResolver, EvalFileEff] r) => ResolverState -> Path Abs Dir -> Sem (PathResolver ': r) a -> Sem r (ResolverState, a)
runPathResolver' st root x = do
  e <- ask
  let _envSingleFile :: Maybe (Path Abs File)
      _envSingleFile
        | e ^. entryPointPackageType == GlobalStdlib = e ^. entryPointModulePath
        | otherwise = Nothing
      env :: ResolverEnv
      env =
        ResolverEnv
          { _envRoot = root,
            _envInitialRoot = root,
            _envLockfileInfo = Nothing,
            _envSingleFile
          }
  runPathResolver2 st env x

runPathResolverPipe' :: (Members '[TaggedLock, Files, Reader EntryPoint, DependencyResolver, Error JuvixError, Error DependencyError, EvalFileEff] r) => ResolverState -> Sem (PathResolver ': r) a -> Sem r (ResolverState, a)
runPathResolverPipe' iniState a = do
  r <- asks (^. entryPointResolverRoot)
  runPathResolver' iniState r a

runPathResolverPipe :: (Members '[TaggedLock, Files, Reader EntryPoint, DependencyResolver, Error JuvixError, Error DependencyError, EvalFileEff] r) => Sem (PathResolver ': r) a -> Sem r (ResolverState, a)
runPathResolverPipe a = do
  r <- asks (^. entryPointResolverRoot)
  runPathResolver r a

evalPathResolverPipe :: (Members '[TaggedLock, Files, Reader EntryPoint, DependencyResolver, Error JuvixError, Error DependencyError, EvalFileEff] r) => Sem (PathResolver ': r) a -> Sem r a
evalPathResolverPipe = fmap snd . runPathResolverPipe
