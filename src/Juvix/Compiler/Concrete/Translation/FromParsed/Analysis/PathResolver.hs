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
import Juvix.Compiler.Concrete.Data.Name
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Base
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Error
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.PackageInfo
import Juvix.Compiler.Pipeline.EntryPoint
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
    _envSingleFile :: Maybe (Path Abs File)
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

withEnvRoot :: Members '[Reader ResolverEnv] r => Path Abs Dir -> Sem r a -> Sem r a
withEnvRoot root' = local (set envRoot root')

mkPackageInfo ::
  forall r.
  (Members '[Files, Error Text, Reader ResolverEnv] r) =>
  Maybe EntryPoint ->
  Path Abs Dir ->
  Sem r PackageInfo
mkPackageInfo mpackageEntry _packageRoot = do
  let buildDir :: Path Abs Dir = maybe (rootBuildDir _packageRoot) (someBaseToAbs _packageRoot . (^. entryPointBuildDir)) mpackageEntry
      buildDirDep :: Maybe (SomeBase Dir)
        | isJust mpackageEntry = Just (Abs buildDir)
        | otherwise = Nothing

  _packagePackage <- maybe (readPackage _packageRoot buildDirDep) (return . (^. entryPointPackage)) mpackageEntry
  let deps :: [Dependency] = _packagePackage ^. packageDependencies
  depsPaths <- mapM getDependencyPath deps
  ensureStdlib _packageRoot buildDir deps
  files :: [Path Rel File] <-
    map (fromJust . stripProperPrefix _packageRoot) <$> walkDirRelAccum juvixAccum _packageRoot []
  let _packageRelativeFiles = HashSet.fromList files
      _packageAvailableRoots =
        HashSet.fromList (_packageRoot : depsPaths)
  return PackageInfo {..}
  where
    juvixAccum :: Path Abs Dir -> [Path Rel Dir] -> [Path Rel File] -> [Path Abs File] -> Sem r ([Path Abs File], Recurse Rel)
    juvixAccum cd _ files acc = return (newJuvixFiles <> acc, RecurseFilter (\hasJuvixYaml d -> not hasJuvixYaml && not (isHiddenDirectory d)))
      where
        newJuvixFiles :: [Path Abs File]
        newJuvixFiles = [cd <//> f | f <- files, isJuvixFile f]

dependencyCached :: (Members '[State ResolverState, Reader ResolverEnv, Files] r) => Dependency -> Sem r Bool
dependencyCached d = do
  p <- getDependencyPath d
  HashMap.member p <$> gets (^. resolverPackages)

withPathFile :: (Members '[PathResolver] r) => TopModulePath -> (Either PathResolverError (Path Abs File) -> Sem r a) -> Sem r a
withPathFile m f = withPath m (f . mapRight (uncurry (<//>)))

getDependencyPath :: Members '[Reader ResolverEnv, Files] r => Dependency -> Sem r (Path Abs Dir)
getDependencyPath = \case
  DependencyPath p -> do
    r <- asks (^. envRoot)
    canonicalDir r (p ^. pathDependencyPath)
  DependencyGit {} -> error "git dependency is not currently supported"

registerDependencies' ::
  (Members '[Reader EntryPoint, State ResolverState, Reader ResolverEnv, Files, Error Text] r) =>
  Sem r ()
registerDependencies' = do
  e <- ask @EntryPoint
  isGlobal <- asks (^. entryPointPackageGlobal)
  if
      | isGlobal -> do
          glob <- globalRoot
          let globDep = mkPathDependency (toFilePath glob)
          addDependency' globDep
      | otherwise -> addDependency' (mkPathDependency (toFilePath (e ^. entryPointRoot)))

addDependency' ::
  (Members '[State ResolverState, Reader ResolverEnv, Files, Error Text] r) =>
  Dependency ->
  Sem r ()
addDependency' = addDependencyHelper Nothing

addDependencyHelper ::
  (Members '[State ResolverState, Reader ResolverEnv, Files, Error Text] r) =>
  Maybe EntryPoint ->
  Dependency ->
  Sem r ()
addDependencyHelper me d = do
  p <- getDependencyPath d
  unlessM (dependencyCached d) $ withEnvRoot p $ do
    pkgInfo <- mkPackageInfo me p
    modify' (set (resolverPackages . at p) (Just pkgInfo))
    forM_ (pkgInfo ^. packageRelativeFiles) $ \f -> do
      modify' (over resolverFiles (HashMap.insertWith (<>) f (pure pkgInfo)))
    forM_ (pkgInfo ^. packagePackage . packageDependencies) addDependency'

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

expectedPath' :: Members '[Reader ResolverEnv] r => Path Abs File -> TopModulePath -> Sem r (Maybe (Path Abs File))
expectedPath' actualPath m = do
  root <- asks (^. envRoot)
  msingle <- asks (^. envSingleFile)
  if
      | msingle == Just actualPath -> return Nothing
      | otherwise -> return (Just (root <//> topModulePathToRelativePath' m))

re ::
  forall r a.
  (Members '[Reader EntryPoint, Files, Error Text] r) =>
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

evalPathResolver' :: (Members '[Reader EntryPoint, Files, Error Text] r) => ResolverState -> Path Abs Dir -> Sem (PathResolver ': r) a -> Sem r a
evalPathResolver' st root = fmap snd . runPathResolver' st root

runPathResolver :: (Members '[Reader EntryPoint, Files, Error Text] r) => Path Abs Dir -> Sem (PathResolver ': r) a -> Sem r (ResolverState, a)
runPathResolver = runPathResolver' iniResolverState

runPathResolver' :: (Members '[Reader EntryPoint, Files, Error Text] r) => ResolverState -> Path Abs Dir -> Sem (PathResolver ': r) a -> Sem r (ResolverState, a)
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
            _envSingleFile
          }
  runState st (runReader env (re x))

runPathResolverPipe' :: (Members '[Files, Reader EntryPoint] r) => ResolverState -> Sem (PathResolver ': r) a -> Sem r (ResolverState, a)
runPathResolverPipe' iniState a = do
  r <- asks (^. entryPointResolverRoot)
  runError (runPathResolver' iniState r (raiseUnder a)) >>= either error return

runPathResolverPipe :: (Members '[Files, Reader EntryPoint] r) => Sem (PathResolver ': r) a -> Sem r (ResolverState, a)
runPathResolverPipe a = do
  r <- asks (^. entryPointResolverRoot)
  runError (runPathResolver r (raiseUnder a)) >>= either error return

evalPathResolverPipe :: (Members '[Files, Reader EntryPoint] r) => Sem (PathResolver ': r) a -> Sem r a
evalPathResolverPipe = fmap snd . runPathResolverPipe
