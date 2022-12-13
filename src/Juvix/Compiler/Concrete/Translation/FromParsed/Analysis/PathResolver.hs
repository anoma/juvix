module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
  ( module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Base,
    module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Error,
    PathResolver,
    addDependency,
    withPath,
    withPathFile,
    runPathResolverPipe,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Concrete.Data.Name
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Base
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Error
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.PackageInfo
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Prelude
import Juvix.Extra.Stdlib (ensureStdlib)

data PathResolver m a where
  -- | Currently, we pass (Just entrypoint) only for the current package and Nothing for all dependencies
  AddDependency :: Maybe EntryPoint -> Dependency -> PathResolver m ()
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
    _stateFiles :: HashMap (Path Rel File) (NonEmpty PackageInfo),
    -- | PackageInfos indexed by root
    _statePackages :: HashMap (Path Abs Dir) PackageInfo
  }
  deriving stock (Show)

makeLenses ''ResolverState
makeLenses ''ResolverEnv

iniResolverState :: ResolverState
iniResolverState =
  ResolverState
    { _statePackages = mempty,
      _stateFiles = mempty
    }

-- TODO use entrypoint to determine whether stdlib needs to be added
-- stdlib files need to be copied if necessary
mkPackageInfo ::
  forall r.
  Members '[Files, Error Text] r =>
  Maybe EntryPoint ->
  Path Abs Dir ->
  Sem r PackageInfo
mkPackageInfo mpackageEntry _packageRoot = do
  _packagePackage <- maybe (readPackage _packageRoot) (return . (^. entryPointPackage)) mpackageEntry
  let deps :: [Dependency] = _packagePackage ^. packageDependencies
  ensureStdlib _packageRoot deps
  files :: [Path Rel File] <- map (fromJust . stripProperPrefix _packageRoot) <$> walkDirRelAccum juvixAccum _packageRoot []
  let _packageRelativeFiles = HashSet.fromList files
      _packageAvailableRoots =
        HashSet.fromList (_packageRoot : map (^. dependencyPath) deps)
  return PackageInfo {..}
  where
    juvixAccum :: Path Abs Dir -> [Path Rel Dir] -> [Path Rel File] -> [Path Abs File] -> Sem r ([Path Abs File], Recurse Rel)
    juvixAccum cd _ files acc = return (newJuvixFiles <> acc, RecurseFilter (not . isHiddenDirectory))
      where
        newJuvixFiles :: [Path Abs File]
        newJuvixFiles = [cd <//> f | f <- files, isJuvixFile f]

dependencyCached :: Members '[State ResolverState] r => Dependency -> Sem r Bool
dependencyCached d = HashMap.member (d ^. dependencyPath) <$> gets (^. statePackages)

withPathFile :: Members '[PathResolver] r => TopModulePath -> (Either PathResolverError (Path Abs File) -> Sem r a) -> Sem r a
withPathFile m f = withPath m (f . mapRight (uncurry (<//>)))

addDependency' ::
  Members '[State ResolverState, Files, Error Text] r =>
  Maybe EntryPoint ->
  Dependency ->
  Sem r ()
addDependency' me d = do
  let p :: Path Abs Dir = d ^. dependencyPath
  unlessM (dependencyCached d) $ do
    pkgInfo <- mkPackageInfo me p
    -- traceM ("adding dependency " <> pack (toFilePath p))
    -- traceM ("has " <> prettyText (pkgInfo ^. packageRelativeFiles . to HashSet.size) <> " files")
    modify' (set (statePackages . at p) (Just pkgInfo))
    forM_ (pkgInfo ^. packageRelativeFiles) $ \f -> do
      -- traceM ("adding file " <> pack (toFilePath f))
      modify' (over stateFiles (HashMap.insertWith (<>) f (pure pkgInfo)))
    forM_ (pkgInfo ^. packagePackage . packageDependencies) (addDependency' Nothing)

currentPackage :: Members '[State ResolverState, Reader ResolverEnv] r => Sem r PackageInfo
currentPackage = do
  curRoot <- asks (^. envRoot)
  gets (^?! statePackages . at curRoot . _Just)

-- | Returns the root of the package where the module belongs and the path to
-- the module relative to the root.
resolvePath' :: Members '[State ResolverState, Reader ResolverEnv] r => TopModulePath -> Sem r (Either PathResolverError (Path Abs Dir, Path Rel File))
resolvePath' mp = do
  z <- gets (^. stateFiles)
  curPkg <- currentPackage
  let rel = topModulePathToRelativeFilePath' mp
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

re ::
  forall r a.
  Members '[Files, Error Text] r =>
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
      WithPath m a -> do
        x :: Either PathResolverError (Path Abs Dir, Path Rel File) <- resolvePath' m
        oldroot <- asks (^. envRoot)
        x' <- pureT x
        a' <- bindT a
        st' <- get
        let root' = case x of
              Left {} -> oldroot
              Right (r, _) -> r
        raise (runPathResolver' st' root' (a' x'))

runPathResolver' :: Members '[Files, Error Text] r => ResolverState -> Path Abs Dir -> Sem (PathResolver ': r) a -> Sem r a
runPathResolver' st root = evalState st . runReader (ResolverEnv root) . re

runPathResolver :: Members '[Files, Error Text] r => Path Abs Dir -> Sem (PathResolver ': r) a -> Sem r a
runPathResolver = runPathResolver' iniResolverState

runPathResolverPipe :: Members '[Files, Reader EntryPoint] r => Sem (PathResolver ': r) a -> Sem r a
runPathResolverPipe a = do
  r <- asks (^. entryPointRoot)
  runError (runPathResolver r (raiseUnder a)) >>= either error return
