module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
  ( module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Base,
    module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Error,
    module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.PackageInfo,
    PathResolver,
    addDependency,
    withPath,
    withPathFile,
    expectedModulePath,
    runPathResolverPipe,
    evalPathResolverPipe,
    ResolverState,
    resolverFiles,
    resolverPackages,
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
  -- | Currently, we pass (Just entrypoint) only for the current package and Nothing for all dependencies
  AddDependency :: Maybe EntryPoint -> Dependency -> PathResolver m ()
  ExpectedModulePath :: TopModulePath -> PathResolver m (Path Abs File)
  WithPath ::
    TopModulePath ->
    (Either PathResolverError (Path Abs Dir, Path Rel File) -> m x) ->
    PathResolver m x

makeSem ''PathResolver

newtype ResolverEnv = ResolverEnv
  { _envRoot :: Path Abs Dir
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

mkPackageInfo ::
  forall r.
  (Members '[Files, Error Text] r) =>
  Maybe EntryPoint ->
  Path Abs Dir ->
  Sem r PackageInfo
mkPackageInfo mpackageEntry _packageRoot = do
  let buildDir = maybe (rootBuildDir _packageRoot) (^. entryPointBuildDir) mpackageEntry
  _packagePackage <- maybe (readPackage _packageRoot buildDir) (return . (^. entryPointPackage)) mpackageEntry
  let deps :: [Dependency] = _packagePackage ^. packageDependencies
  ensureStdlib buildDir deps
  files :: [Path Rel File] <- map (fromJust . stripProperPrefix _packageRoot) <$> walkDirRelAccum juvixAccum _packageRoot []
  let _packageRelativeFiles = HashSet.fromList files
      _packageAvailableRoots =
        HashSet.fromList (_packageRoot : map (^. dependencyPath) deps)
  return PackageInfo {..}
  where
    juvixAccum :: Path Abs Dir -> [Path Rel Dir] -> [Path Rel File] -> [Path Abs File] -> Sem r ([Path Abs File], Recurse Rel)
    juvixAccum cd _ files acc = return (newJuvixFiles <> acc, RecurseFilter (\hasJuvixYaml d -> not hasJuvixYaml || not (isHiddenDirectory d)))
      where
        newJuvixFiles :: [Path Abs File]
        newJuvixFiles = [cd <//> f | f <- files, isJuvixFile f]

dependencyCached :: (Members '[State ResolverState] r) => Dependency -> Sem r Bool
dependencyCached d = HashMap.member (d ^. dependencyPath) <$> gets (^. resolverPackages)

withPathFile :: (Members '[PathResolver] r) => TopModulePath -> (Either PathResolverError (Path Abs File) -> Sem r a) -> Sem r a
withPathFile m f = withPath m (f . mapRight (uncurry (<//>)))

addDependency' ::
  (Members '[State ResolverState, Files, Error Text] r) =>
  Maybe EntryPoint ->
  Dependency ->
  Sem r ()
addDependency' me d = do
  let p :: Path Abs Dir = d ^. dependencyPath
  unlessM (dependencyCached d) $ do
    pkgInfo <- mkPackageInfo me p
    -- traceM ("adding dependency " <> pack (toFilePath p))
    -- traceM ("has " <> prettyText (pkgInfo ^. packageRelativeFiles . to HashSet.size) <> " files")
    modify' (set (resolverPackages . at p) (Just pkgInfo))
    forM_ (pkgInfo ^. packageRelativeFiles) $ \f -> do
      -- traceM ("adding file " <> pack (toFilePath f))
      modify' (over resolverFiles (HashMap.insertWith (<>) f (pure pkgInfo)))
    forM_ (pkgInfo ^. packagePackage . packageDependencies) (addDependency' Nothing)

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

expectedPath' :: (Members '[Reader ResolverEnv] r) => TopModulePath -> Sem r (Path Abs File)
expectedPath' m = do
  root <- asks (^. envRoot)
  return (root <//> topModulePathToRelativePath' m)

re ::
  forall r a.
  (Members '[Files, Error Text] r) =>
  Sem (PathResolver ': r) a ->
  Sem (Reader ResolverEnv ': State ResolverState ': r) a
re = reinterpret2H helper
  where
    helper ::
      forall rInitial x.
      PathResolver (Sem rInitial) x ->
      Tactical PathResolver (Sem rInitial) (Reader ResolverEnv ': (State ResolverState ': r)) x
    helper = \case
      AddDependency me m -> addDependency' me m >>= pureT
      ExpectedModulePath m -> expectedPath' m >>= pureT
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

evalPathResolver' :: (Members '[Files, Error Text] r) => ResolverState -> Path Abs Dir -> Sem (PathResolver ': r) a -> Sem r a
evalPathResolver' st root = fmap snd . runPathResolver' st root

runPathResolver' :: (Members '[Files, Error Text] r) => ResolverState -> Path Abs Dir -> Sem (PathResolver ': r) a -> Sem r (ResolverState, a)
runPathResolver' st root x = do
  runState st (runReader (ResolverEnv root) (re x))

runPathResolver :: (Members '[Files, Error Text] r) => Path Abs Dir -> Sem (PathResolver ': r) a -> Sem r (ResolverState, a)
runPathResolver = runPathResolver' iniResolverState

runPathResolverPipe :: (Members '[Files, Reader EntryPoint] r) => Sem (PathResolver ': r) a -> Sem r (ResolverState, a)
runPathResolverPipe a = do
  r <- asks (^. entryPointResolverRoot)
  runError (runPathResolver r (raiseUnder a)) >>= either error return

evalPathResolverPipe :: (Members '[Files, Reader EntryPoint] r) => Sem (PathResolver ': r) a -> Sem r a
evalPathResolverPipe = fmap snd . runPathResolverPipe
