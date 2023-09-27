module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
  ( module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Base,
    module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Error,
    module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.PackageInfo,
    PathResolver,
    registerDependencies,
    withPath,
    withPathFile,
    expectedModulePath,
    runPathResolver,
    runPathResolverPipe,
    runPathResolverPipe',
    evalPathResolverPipe,
    ResolverState,
    resolverFiles,
    resolverPackages,
    iniResolverState,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Text qualified as T
import Juvix.Compiler.Concrete.Data.Name
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Base
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Error
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.PackageInfo
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Lockfile
import Juvix.Data.Effect.Git
import Juvix.Extra.Paths
import Juvix.Extra.Stdlib (ensureStdlib)
import Juvix.Prelude

data PathResolver m a where
  RegisterDependencies :: PathResolver m ()
  ExpectedModulePath :: Path Abs File -> TopModulePath -> PathResolver m (Maybe (Path Abs File))
  WithPath ::
    TopModulePath ->
    (Either PathResolverError (Path Abs Dir, Path Rel File) -> m x) ->
    PathResolver m x

makeSem ''PathResolver

data ResolverEnv = ResolverEnv
  { _envRoot :: Path Abs Dir,
    -- | The path of the input file *if* we are using the global project
    _envSingleFile :: Maybe (Path Abs File),
    _envLockfile :: Maybe Lockfile
  }

data ResolverState = ResolverState
  { -- | juvix files indexed by relative path
    _resolverFiles :: HashMap (Path Rel File) (NonEmpty PackageInfo),
    -- | PackageInfos indexed by root
    _resolverPackages :: HashMap (Path Abs Dir) PackageInfo
  }
  deriving stock (Show)

makeLenses ''ResolverState
makeLenses ''ResolverEnv

iniResolverState :: ResolverState
iniResolverState =
  ResolverState
    { _resolverPackages = mempty,
      _resolverFiles = mempty
    }

withEnvRoot :: (Members '[Reader ResolverEnv] r) => Path Abs Dir -> Sem r a -> Sem r a
withEnvRoot root' = local (set envRoot root')

withLockfile :: (Members '[Reader ResolverEnv] r) => Lockfile -> Sem r a -> Sem r a
withLockfile f = local (set envLockfile (Just f))

mkPackage ::
  forall r.
  (Members '[Files, Error Text, Reader ResolverEnv, GitClone] r) =>
  Maybe EntryPoint ->
  Path Abs Dir ->
  Sem r Package
mkPackage mpackageEntry _packageRoot = do
  let buildDir :: Path Abs Dir = maybe (rootBuildDir _packageRoot) (someBaseToAbs _packageRoot . (^. entryPointBuildDir)) mpackageEntry
      buildDirDep :: BuildDir
        | isJust mpackageEntry = CustomBuildDir (Abs buildDir)
        | otherwise = DefaultBuildDir
  maybe (readPackage _packageRoot buildDirDep) (return . (^. entryPointPackage)) mpackageEntry

mkPackageInfo ::
  forall r.
  (Members '[Files, Error Text, Reader ResolverEnv, Error DependencyError, GitClone] r) =>
  Maybe EntryPoint ->
  Path Abs Dir ->
  Package ->
  Sem r PackageInfo
mkPackageInfo mpackageEntry _packageRoot _packagePackage = do
  let buildDir :: Path Abs Dir = maybe (rootBuildDir _packageRoot) (someBaseToAbs _packageRoot . (^. entryPointBuildDir)) mpackageEntry
  deps <- resolveDependencies
  depsPaths <- mapM (getDependencyPath . mkPackageDependencyInfo (_packagePackage ^. packageFile)) deps
  ensureStdlib _packageRoot buildDir deps
  files :: [Path Rel File] <-
    map (fromJust . stripProperPrefix _packageRoot) <$> walkDirRelAccum juvixAccum _packageRoot []
  let _packageRelativeFiles = HashSet.fromList files
      _packageAvailableRoots =
        HashSet.fromList (_packageRoot : depsPaths)
  return PackageInfo {_packageResolvedDependencies = deps, ..}
  where
    juvixAccum :: Path Abs Dir -> [Path Rel Dir] -> [Path Rel File] -> [Path Abs File] -> Sem r ([Path Abs File], Recurse Rel)
    juvixAccum cd _ files acc = return (newJuvixFiles <> acc, RecurseFilter (\hasJuvixYaml d -> not hasJuvixYaml && not (isHiddenDirectory d)))
      where
        newJuvixFiles :: [Path Abs File]
        newJuvixFiles = [cd <//> f | f <- files, isJuvixFile f]

    resolveDependencies :: Sem r [Dependency]
    resolveDependencies = do
      mlockfile <- asks (^. envLockfile)
      return $ case mlockfile of
        Nothing -> _packagePackage ^. packageDependencies
        Just lf -> (^. lockfileDependencyDependency) <$> lf ^. lockfileDependencies

dependencyCached :: (Members '[State ResolverState, Reader ResolverEnv, Files, GitClone] r) => Path Abs Dir -> Sem r Bool
dependencyCached p = HashMap.member p <$> gets (^. resolverPackages)

withPathFile :: (Members '[PathResolver] r) => TopModulePath -> (Either PathResolverError (Path Abs File) -> Sem r a) -> Sem r a
withPathFile m f = withPath m (f . mapRight (uncurry (<//>)))

getDependencyPath :: forall r. (Members '[Reader ResolverEnv, Files, Error DependencyError, GitClone] r) => PackageDependencyInfo -> Sem r (Path Abs Dir)
getDependencyPath i = case i ^. packageDepdendencyInfoDependency of
  DependencyPath p -> do
    r <- asks (^. envRoot)
    canonicalDir r (p ^. pathDependencyPath)
  DependencyGit g -> do
    r <- rootBuildDir <$> asks (^. envRoot)
    let cloneDir = r <//> relDependenciesDir <//> relDir (T.unpack (g ^. gitDependencyName))
        cloneArgs = CloneArgs {_cloneArgsCloneDir = cloneDir, _cloneArgsRepoUrl = g ^. gitDependencyUrl}
    scoped @CloneArgs @Git cloneArgs $
      fetchOnNoSuchRefAndRetry (errorHandler cloneDir) (`checkout` (g ^. gitDependencyRef)) >> return cloneDir
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

registerDependencies' ::
  (Members '[Reader EntryPoint, State ResolverState, Reader ResolverEnv, Files, Error Text, Error DependencyError, GitClone] r) =>
  Sem r ()
registerDependencies' = do
  e <- ask @EntryPoint
  isGlobal <- asks (^. entryPointPackageGlobal)
  if
      | isGlobal -> do
          glob <- globalRoot
          let globDep = mkPathDependency (toFilePath glob)
              globalPackageFile = mkPackageFilePath glob
          addDependency' (Just e) (mkPackageDependencyInfo globalPackageFile globDep)
      | otherwise -> do
          let f = mkPackageFilePath (e ^. entryPointRoot)
          addDependency' (Just e) (mkPackageDependencyInfo f (mkPathDependency (toFilePath (e ^. entryPointRoot))))

addDependency' ::
  (Members '[State ResolverState, Reader ResolverEnv, Files, Error Text, Error DependencyError, GitClone] r) =>
  Maybe EntryPoint ->
  PackageDependencyInfo ->
  Sem r ()
addDependency' me = addDependencyHelper me

addDependencyHelper ::
  forall r.
  (Members '[State ResolverState, Reader ResolverEnv, Files, Error Text, Error DependencyError, GitClone] r) =>
  Maybe EntryPoint ->
  PackageDependencyInfo ->
  Sem r ()
addDependencyHelper me d = do
  p <- getDependencyPath d
  unlessM (dependencyCached p) $ withEnvRoot p $ do
    pkg <- mkPackage me p
    selectPackageLockfile pkg $ do
      pkgInfo <- mkPackageInfo me p pkg
      modify' (set (resolverPackages . at p) (Just pkgInfo))
      forM_ (pkgInfo ^. packageRelativeFiles) $ \f -> do
        modify' (over resolverFiles (HashMap.insertWith (<>) f (pure pkgInfo)))
      let packagePath = pkgInfo ^. packagePackage . packageFile
      forM_ (pkgInfo ^. packagePackage . packageDependencies) (\dep -> selectDependencyLockfile dep (addDependency' Nothing (mkPackageDependencyInfo packagePath dep)))
  where
    selectPackageLockfile :: Package -> Sem r a -> Sem r a
    selectPackageLockfile pkg action = do
      currentLockfile <- asks (^. envLockfile)
      case currentLockfile of
        Just _ -> action
        Nothing -> case (pkg ^. packageLockfile) of
          Just lf -> withLockfile lf action
          Nothing -> action

    selectDependencyLockfile :: Dependency -> Sem r a -> Sem r a
    selectDependencyLockfile dep action = do
      currentLockfile <- asks (^. envLockfile)
      case currentLockfile of
        Nothing -> action
        Just lf -> case extractLockfile lf dep of
          Just dlf -> withLockfile dlf action
          Nothing -> throw errMsg
      where
        errMsg :: Text
        errMsg = "Could not find dependency: " <> show dep <> " in lockfile"

currentPackage :: (Members '[State ResolverState, Reader ResolverEnv] r) => Sem r PackageInfo
currentPackage = do
  curRoot <- asks (^. envRoot)
  gets (^?! resolverPackages . at curRoot . _Just)

-- | Returns the root of the package where the module belongs and the path to
-- the module relative to the root.
resolvePath' :: (Members '[State ResolverState, Reader ResolverEnv] r) => TopModulePath -> Sem r (Either PathResolverError (Path Abs Dir, Path Rel File))
resolvePath' mp = do
  z <- gets (^. resolverFiles)
  curPkg <- currentPackage
  let rel = topModulePathToRelativePath' mp
      packagesWithModule = z ^. at rel
      visible :: PackageInfo -> Bool
      visible p = HashSet.member (p ^. packageRoot) (curPkg ^. packageAvailableRoots)
  return $ case filter visible (maybe [] toList packagesWithModule) of
    [r] -> Right (r ^. packageRoot, rel)
    [] ->
      Left
        ( ErrMissingModule
            MissingModule
              { _missingInfo = curPkg,
                _missingModule = mp
              }
        )
    (r : rs) ->
      Left
        ( ErrDependencyConflict
            DependencyConflict
              { _conflictPackages = r :| rs,
                _conflictPath = mp
              }
        )

expectedPath' :: (Members '[Reader ResolverEnv] r) => Path Abs File -> TopModulePath -> Sem r (Maybe (Path Abs File))
expectedPath' actualPath m = do
  root <- asks (^. envRoot)
  msingle <- asks (^. envSingleFile)
  if
      | msingle == Just actualPath -> return Nothing
      | otherwise -> return (Just (root <//> topModulePathToRelativePath' m))

re ::
  forall r a.
  (Members '[Reader EntryPoint, Files, Error Text, Error DependencyError, GitClone] r) =>
  Sem (PathResolver ': r) a ->
  Sem (Reader ResolverEnv ': State ResolverState ': r) a
re = reinterpret2H helper
  where
    helper ::
      forall rInitial x.
      PathResolver (Sem rInitial) x ->
      Tactical PathResolver (Sem rInitial) (Reader ResolverEnv ': (State ResolverState ': r)) x
    helper = \case
      RegisterDependencies -> registerDependencies' >>= pureT
      ExpectedModulePath a m -> expectedPath' a m >>= pureT
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

evalPathResolver' :: (Members '[Reader EntryPoint, Files, Error Text, Error DependencyError, GitClone] r) => ResolverState -> Path Abs Dir -> Sem (PathResolver ': r) a -> Sem r a
evalPathResolver' st root = fmap snd . runPathResolver' st root

runPathResolver :: (Members '[Reader EntryPoint, Files, Error Text, Error DependencyError, GitClone] r) => Path Abs Dir -> Sem (PathResolver ': r) a -> Sem r (ResolverState, a)
runPathResolver = runPathResolver' iniResolverState

runPathResolver' :: (Members '[Reader EntryPoint, Files, Error Text, Error DependencyError, GitClone] r) => ResolverState -> Path Abs Dir -> Sem (PathResolver ': r) a -> Sem r (ResolverState, a)
runPathResolver' st root x = do
  e <- ask
  let _envSingleFile :: Maybe (Path Abs File)
      _envSingleFile
        | e ^. entryPointPackageGlobal = e ^? entryPointModulePaths . _head
        | otherwise = Nothing
      env :: ResolverEnv
      env =
        ResolverEnv
          { _envRoot = root,
            _envLockfile = Nothing,
            _envSingleFile
          }
  runState st (runReader env (re x))

runPathResolverPipe' :: (Members '[Files, Reader EntryPoint, Error DependencyError, GitClone] r) => ResolverState -> Sem (PathResolver ': r) a -> Sem r (ResolverState, a)
runPathResolverPipe' iniState a = do
  r <- asks (^. entryPointResolverRoot)
  runError (runPathResolver' iniState r (raiseUnder a)) >>= either error return

runPathResolverPipe :: (Members '[Files, Reader EntryPoint, Error DependencyError, GitClone] r) => Sem (PathResolver ': r) a -> Sem r (ResolverState, a)
runPathResolverPipe a = do
  r <- asks (^. entryPointResolverRoot)
  runError (runPathResolver r (raiseUnder a)) >>= either error return

evalPathResolverPipe :: (Members '[Files, Reader EntryPoint, Error DependencyError, GitClone] r) => Sem (PathResolver ': r) a -> Sem r a
evalPathResolverPipe = fmap snd . runPathResolverPipe
