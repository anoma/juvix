module Juvix.Compiler.Pipeline.Loader.PathResolver
  ( module Juvix.Compiler.Pipeline.Loader.PathResolver.Paths,
    module Juvix.Compiler.Pipeline.Loader.PathResolver.Base,
    module Juvix.Compiler.Pipeline.Loader.PathResolver.Error,
    module Juvix.Compiler.Pipeline.Loader.PathResolver.Data,
    module Juvix.Compiler.Pipeline.Loader.PathResolver.PackageInfo,
    runPathResolver,
    runPathResolverPipe,
    runPathResolverPipe',
    evalPathResolverPipe,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Text qualified as T
import Juvix.Compiler.Concrete.Data.Name
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Loader.PathResolver.Base
import Juvix.Compiler.Pipeline.Loader.PathResolver.Data
import Juvix.Compiler.Pipeline.Loader.PathResolver.Error
import Juvix.Compiler.Pipeline.Loader.PathResolver.PackageInfo
import Juvix.Compiler.Pipeline.Loader.PathResolver.Paths
import Juvix.Compiler.Pipeline.Lockfile
import Juvix.Compiler.Pipeline.Package
import Juvix.Compiler.Pipeline.Package.Loader.EvalEff
import Juvix.Compiler.Pipeline.Root.Base (PackageType (..))
import Juvix.Data.Effect.Git
import Juvix.Data.Effect.TaggedLock
import Juvix.Data.SHA256 qualified as SHA256
import Juvix.Extra.PackageFiles
import Juvix.Extra.Paths
import Juvix.Extra.Stdlib (ensureStdlib)
import Juvix.Prelude

mkPackage ::
  forall r.
  (Members '[Files, Error JuvixError, Reader ResolverEnv, GitClone, EvalFileEff] r) =>
  Maybe EntryPoint ->
  Path Abs Dir ->
  Sem r Package
mkPackage mpackageEntry _packageRoot = do
  let buildDirDep = case mpackageEntry of
        Just packageEntry -> rootedBuildDir _packageRoot (packageEntry ^. entryPointBuildDir)
        Nothing -> DefaultBuildDir
  maybe (readPackage _packageRoot buildDirDep) (return . (^. entryPointPackage)) mpackageEntry

mkPackageInfo ::
  forall r.
  (Members '[TaggedLock, Files, Error JuvixError, Reader ResolverEnv, Error DependencyError, GitClone] r) =>
  Maybe EntryPoint ->
  Path Abs Dir ->
  Package ->
  Sem r PackageInfo
mkPackageInfo mpackageEntry _packageRoot pkg = do
  let buildDir :: Path Abs Dir = maybe (rootBuildDir _packageRoot) (someBaseToAbs _packageRoot . resolveBuildDir . (^. entryPointBuildDir)) mpackageEntry
  deps <- getDependencies
  let _packagePackage = set packageDependencies deps pkg
  depsPaths <- mapM (fmap (^. resolvedDependencyPath) . resolveDependency . mkPackageDependencyInfo pkgFile) deps
  ensureStdlib _packageRoot buildDir deps
  files :: [Path Rel File] <-
    map (fromJust . stripProperPrefix _packageRoot) <$> walkDirRelAccum juvixAccum _packageRoot []
  packageBaseAbsDir <- globalPackageBaseRoot
  let _packageRelativeFiles = HashSet.fromList files
      _packageAvailableRoots =
        HashSet.fromList (packageBaseAbsDir : _packageRoot : depsPaths)
  return PackageInfo {..}
  where
    juvixAccum :: Path Abs Dir -> [Path Rel Dir] -> [Path Rel File] -> [Path Abs File] -> Sem r ([Path Abs File], Recurse Rel)
    juvixAccum cd _ files acc = return (newJuvixFiles <> acc, RecurseFilter (\hasJuvixPackage d -> not hasJuvixPackage && not (isHiddenDirectory d)))
      where
        newJuvixFiles :: [Path Abs File]
        newJuvixFiles = [cd <//> f | f <- files, isJuvixFile f || isJuvixMarkdownFile f]

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

lookupCachedDependency :: (Members '[State ResolverState, Reader ResolverEnv, Files, GitClone] r) => Path Abs Dir -> Sem r (Maybe LockfileDependency)
lookupCachedDependency p = fmap (^. resolverCacheItemDependency) . HashMap.lookup p <$> gets (^. resolverCache)

resolveDependency :: forall r. (Members '[Reader ResolverEnv, Files, Error DependencyError, GitClone] r) => PackageDependencyInfo -> Sem r ResolvedDependency
resolveDependency i = case i ^. packageDepdendencyInfoDependency of
  DependencyPath p -> do
    r <- asks (^. envRoot)
    p' <- canonicalDir r (p ^. pathDependencyPath)
    return
      ResolvedDependency
        { _resolvedDependencyPath = p',
          _resolvedDependencyDependency = i ^. packageDepdendencyInfoDependency
        }
  DependencyGit g -> do
    r <- rootBuildDir <$> asks (^. envRoot)
    let cloneDir = r <//> relDependenciesDir <//> relDir (T.unpack (g ^. gitDependencyName))
        cloneArgs =
          CloneArgs
            { _cloneArgsCloneDir = cloneDir,
              _cloneArgsRepoUrl = g ^. gitDependencyUrl
            }
    scoped cloneArgs $ do
      fetchOnNoSuchRefAndRetry (errorHandler cloneDir) (`checkout` (g ^. gitDependencyRef))
      resolvedRef <- headRef (errorHandler cloneDir)
      return
        ResolvedDependency
          { _resolvedDependencyPath = cloneDir,
            _resolvedDependencyDependency =
              DependencyGit (set gitDependencyRef resolvedRef g)
          }
    where
      errorHandler :: Path Abs Dir -> GitError -> Sem (Git ': r) a
      errorHandler p c =
        throw
          DependencyError
            { _dependencyErrorCause =
                GitDependencyError
                  DependencyErrorGit
                    { _dependencyErrorGitCloneDir = p,
                      _dependencyErrorGitError = c
                    },
              _dependencyErrorPackageFile = i ^. packageDependencyInfoPackageFile
            }

registerPackageBase ::
  forall r.
  (Members '[TaggedLock, State ResolverState, Reader ResolverEnv, Files] r) =>
  Sem r ()
registerPackageBase = do
  packageBaseAbsDir <- globalPackageBaseRoot
  runReader packageBaseAbsDir updatePackageBaseFiles
  packageBaseRelFiles <- relFiles packageBaseAbsDir
  let pkgInfo =
        PackageInfo
          { _packageRoot = packageBaseAbsDir,
            _packageRelativeFiles = packageBaseRelFiles,
            _packagePackage = packageBasePackage,
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
  forM_ (pkgInfo ^. packageRelativeFiles) $ \f -> do
    modify' (over resolverFiles (HashMap.insertWith (<>) f (pure pkgInfo)))

registerDependencies' ::
  forall r.
  (Members '[TaggedLock, Reader EntryPoint, State ResolverState, Reader ResolverEnv, Files, Error JuvixError, Error DependencyError, GitClone, EvalFileEff] r) =>
  DependenciesConfig ->
  Sem r ()
registerDependencies' conf = do
  e <- ask @EntryPoint
  registerPackageBase
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
    shouldWriteLockfile :: Sem r Bool
    shouldWriteLockfile = do
      lockfileExists <- lockfilePath >>= fileExists'
      hasRemoteDependencies <- gets (^. resolverHasRemoteDependencies)
      shouldUpdateLockfile' <- gets (^. resolverShouldUpdateLockfile)

      let shouldForce = conf ^. dependenciesConfigForceUpdateLockfile
          shouldWriteInitialLockfile = not lockfileExists && hasRemoteDependencies
          shouldUpdateLockfile = lockfileExists && shouldUpdateLockfile'
      return (shouldForce || shouldWriteInitialLockfile || shouldUpdateLockfile)

    lockfilePath :: Sem r (Path Abs File)
    lockfilePath = do
      root <- asks (^. envRoot)
      return (mkPackageLockfilePath root)

addRootDependency ::
  forall r.
  (Members '[TaggedLock, State ResolverState, Reader ResolverEnv, Files, Error JuvixError, Error DependencyError, GitClone, EvalFileEff] r) =>
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
  withEnvRoot p $ do
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
  (Members '[TaggedLock, State ResolverState, Reader ResolverEnv, Files, Error JuvixError, Error DependencyError, GitClone, EvalFileEff] r) =>
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

addDependency' ::
  forall r.
  (Members '[TaggedLock, State ResolverState, Reader ResolverEnv, Files, Error JuvixError, Error DependencyError, GitClone, EvalFileEff] r) =>
  Package ->
  Maybe EntryPoint ->
  ResolvedDependency ->
  Sem r LockfileDependency
addDependency' pkg me resolvedDependency = do
  selectPackageLockfile pkg $ do
    pkgInfo <- mkPackageInfo me (resolvedDependency ^. resolvedDependencyPath) pkg
    forM_ (pkgInfo ^. packageRelativeFiles) $ \f -> do
      modify' (over resolverFiles (HashMap.insertWith (<>) f (pure pkgInfo)))
    let packagePath = pkgInfo ^. packagePackage . packageFile
    subDeps <-
      forM
        (pkgInfo ^. packagePackage . packageDependencies)
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
        Nothing -> case (p ^. packageLockfile) of
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

-- | Returns the root of the package where the module belongs and the path to
-- the module relative to the root.
resolvePath' :: (Members '[Files, State ResolverState, Reader ResolverEnv] r) => TopModulePath -> Sem r (Either PathResolverError (Path Abs Dir, Path Rel File))
resolvePath' mp = do
  z <- gets (^. resolverFiles)
  curPkg <- currentPackage
  let exts = [FileExtJuvix, FileExtJuvixMarkdown]
  let rpaths = map (`topModulePathToRelativePathByExt` mp) exts

      packagesWithModule :: [(PackageInfo, Path Rel File)]
      packagesWithModule =
        [ (pkg, p)
          | p <- rpaths,
            pkgs <- toList (HashMap.lookup p z),
            pkg <- toList pkgs,
            visible pkg
        ]

      visible :: PackageInfo -> Bool
      visible pkg = HashSet.member (pkg ^. packageRoot) (curPkg ^. packageAvailableRoots)
  return $ case packagesWithModule of
    [(r, relPath)] -> Right (r ^. packageRoot, relPath)
    [] ->
      Left
        ( ErrMissingModule
            MissingModule
              { _missingInfo = curPkg,
                _missingModule = mp
              }
        )
    ((r, _) : rs) ->
      Left
        ( ErrDependencyConflict
            DependencyConflict
              { _conflictPackages = r :| map fst rs,
                _conflictPath = mp
              }
        )

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

re ::
  forall r a.
  (Members '[TaggedLock, Reader EntryPoint, Files, Error JuvixError, Error DependencyError, GitClone, EvalFileEff] r) =>
  Sem (PathResolver ': r) a ->
  Sem (Reader ResolverEnv ': State ResolverState ': r) a
re = reinterpret2H helper
  where
    helper ::
      forall rInitial x.
      PathResolver (Sem rInitial) x ->
      Tactical PathResolver (Sem rInitial) (Reader ResolverEnv ': (State ResolverState ': r)) x
    helper = \case
      RegisterDependencies forceUpdateLockfile -> registerDependencies' forceUpdateLockfile >>= pureT
      ExpectedPathInfoTopModule m -> expectedPath' m >>= pureT
      WithPath m a -> do
        x :: Either PathResolverError (Path Abs Dir, Path Rel File) <- resolvePath' m
        oldroot <- asks (^. envRoot)
        x' <- pureT x
        a' <- bindT a
        st' <- get
        let root' = case x of
              Left {} -> oldroot
              Right (r, _) -> r
        raise (evalPathResolver' st' root' (a' x'))

evalPathResolver' :: (Members '[TaggedLock, Reader EntryPoint, Files, Error JuvixError, Error DependencyError, GitClone, EvalFileEff] r) => ResolverState -> Path Abs Dir -> Sem (PathResolver ': r) a -> Sem r a
evalPathResolver' st root = fmap snd . runPathResolver' st root

runPathResolver :: (Members '[TaggedLock, Reader EntryPoint, Files, Error JuvixError, Error DependencyError, GitClone, EvalFileEff] r) => Path Abs Dir -> Sem (PathResolver ': r) a -> Sem r (ResolverState, a)
runPathResolver = runPathResolver' iniResolverState

runPathResolver' :: (Members '[TaggedLock, Reader EntryPoint, Files, Error JuvixError, Error DependencyError, GitClone, EvalFileEff] r) => ResolverState -> Path Abs Dir -> Sem (PathResolver ': r) a -> Sem r (ResolverState, a)
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
            _envLockfileInfo = Nothing,
            _envSingleFile
          }
  runState st (runReader env (re x))

runPathResolverPipe' :: (Members '[TaggedLock, Files, Reader EntryPoint, Error DependencyError, GitClone, Error JuvixError, EvalFileEff] r) => ResolverState -> Sem (PathResolver ': r) a -> Sem r (ResolverState, a)
runPathResolverPipe' iniState a = do
  r <- asks (^. entryPointResolverRoot)
  runPathResolver' iniState r a

runPathResolverPipe :: (Members '[TaggedLock, Files, Reader EntryPoint, Error DependencyError, GitClone, Error JuvixError, EvalFileEff] r) => Sem (PathResolver ': r) a -> Sem r (ResolverState, a)
runPathResolverPipe a = do
  r <- asks (^. entryPointResolverRoot)
  runPathResolver r a

evalPathResolverPipe :: (Members '[TaggedLock, Files, Reader EntryPoint, Error DependencyError, GitClone, Error JuvixError, EvalFileEff] r) => Sem (PathResolver ': r) a -> Sem r a
evalPathResolverPipe = fmap snd . runPathResolverPipe
