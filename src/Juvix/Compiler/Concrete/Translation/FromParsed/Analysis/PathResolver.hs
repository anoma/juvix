module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
  ( module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Base,
    module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Error,
    PathResolver,
    addDependency,
    -- resolvePath,
    withPath,
    -- runPathResolver,
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
import Juvix.Prelude.Pretty
import System.FilePath.Find as Find

data PathResolver m a where
  AddDependency :: Dependency -> PathResolver m ()
  WithPath :: TopModulePath -> (Either PathResolverError FilePath -> m a) -> PathResolver m a

makeSem ''PathResolver

newtype ResolverEnv = ResolverEnv
  { _envRoot :: FilePath
  }

data ResolverState = ResolverState
  { -- | juvix files indexed by relative path
    _stateFiles :: HashMap FilePath (NonEmpty PackageInfo),
    -- | PackageInfos indexed by root
    _statePackages :: HashMap FilePath PackageInfo
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

-- | the argument should be an absolute path
mkPackageInfo :: Members '[Files, Error Text] r => FilePath -> Sem r PackageInfo
mkPackageInfo _packageRoot = do
  _packagePackage <- readPackage _packageRoot
  fs <- map (normalise . makeRelative _packageRoot) <$> filesFind recur (extension ==? ".juvix") _packageRoot
  let _packageRelativeFiles = HashSet.fromList fs
      _packageAvailableRoots =
        HashSet.fromList (_packageRoot : map (^. dependencyPath) (_packagePackage ^. packageDependencies))
  return PackageInfo {..}
  where
    recur :: FindClause Bool
    recur = notHidden <$> fileName
      where
        notHidden = \case
          [] -> True
          (h : _) -> h /= '.'

dependencyCached :: Members '[State ResolverState] r => Dependency -> Sem r Bool
dependencyCached d = HashMap.member (d ^. dependencyPath) <$> gets (^. statePackages)

addDependency' :: Members '[State ResolverState, Files, Error Text] r => Dependency -> Sem r ()
addDependency' d = do
  let p = d ^. dependencyPath
  unlessM (dependencyCached d) $ do
    pkgInfo <- mkPackageInfo p
    traceM ("adding dependency " <> pack p)
    traceM ("has " <> prettyText (pkgInfo ^. packageRelativeFiles . to HashSet.size) <> " files")
    modify' (set (statePackages . at p) (Just pkgInfo))
    forM_ (pkgInfo ^. packageRelativeFiles) $ \f -> do
      traceM ("adding file " <> pack f)
      modify' (over stateFiles (HashMap.insertWith (<>) f (pure pkgInfo)))
    forM_ (pkgInfo ^. packagePackage . packageDependencies) addDependency'

currentPackage :: Members '[State ResolverState, Reader ResolverEnv] r => Sem r PackageInfo
currentPackage = do
  curRoot <- asks (^. envRoot)
  gets (^?! statePackages . at curRoot . _Just)

resolvePath' :: Members '[State ResolverState, Reader ResolverEnv] r => TopModulePath -> Sem r (Either PathResolverError FilePath)
resolvePath' mp = do
  z <- gets (^. stateFiles)
  curPkg <- currentPackage
  let rel = topModulePathToRelativeFilePath' mp
      m = z ^. at rel
      visible :: PackageInfo -> Bool
      visible p = HashSet.member (p ^. packageRoot) (curPkg ^. packageAvailableRoots)
  return $ case filter visible (maybe [] toList m) of
    [r] -> Right (r ^. packageRoot </> rel)
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
      AddDependency m -> addDependency' m >>= pureT
      WithPath m a -> do
        x :: Either PathResolverError FilePath <- resolvePath' m
        root <- asks (^. envRoot)
        x' <- pureT x
        a' <- bindT a
        st' <- get
        let root' = fromRight root x
        raise (runPathResolver' st' root' (a' x'))

runPathResolver' :: Members '[Files, Error Text] r => ResolverState -> FilePath -> Sem (PathResolver ': r) a -> Sem r a
runPathResolver' st root = evalState st . runReader (ResolverEnv root) . re

runPathResolver :: Members '[Files, Error Text] r => FilePath -> Sem (PathResolver ': r) a -> Sem r a
runPathResolver = runPathResolver' iniResolverState

runPathResolverPipe :: Members '[Files, Reader EntryPoint] r => Sem (PathResolver ': r) a -> Sem r a
runPathResolverPipe a = do
  r <- asks (^. entryPointRoot)
  runError (runPathResolver r (raiseUnder a)) >>= either error return
