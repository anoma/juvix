module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
  ( PathResolver,
    addDependency,
    -- resolvePath,
    withPath,
    runPathResolver,
    runPathResolverPipe,
    topModulePathToRelativeFilePath,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Concrete.Data.Name
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Prelude
import System.FilePath.Find as Find

data PathResolver m a where
  AddDependency :: Dependency -> PathResolver m ()
  WithPath :: TopModulePath -> (Either FilesError FilePath -> m a) -> PathResolver m a

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

data PackageInfo = PackageInfo
  { _packageRoot :: FilePath,
    _packageRelativeFiles :: HashSet FilePath,
    _packagePackage :: Package
  }

makeLenses ''PackageInfo
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
  fs <- map (normalise . makeRelative _packageRoot) <$> filesFind always (extension ==? ".juvix") _packageRoot
  let _packageRelativeFiles = HashSet.fromList fs
  return PackageInfo {..}

dependencyCached :: Members '[State ResolverState] r => Dependency -> Sem r Bool
dependencyCached d = HashMap.member (d ^. dependencyPath) <$> gets (^. statePackages)

addDependency' :: Members '[State ResolverState, Files, Error Text] r => Dependency -> Sem r ()
addDependency' d = do
  let p = d ^. dependencyPath
  unlessM (dependencyCached d) $ do
    pkgInfo <- mkPackageInfo p
    modify' (set (statePackages . at p) (Just pkgInfo))
    forM_ (pkgInfo ^. packageRelativeFiles) $ \f ->
      modify' (over (stateFiles . at f) (Just . maybe (pure pkgInfo) (NonEmpty.cons pkgInfo)))
    forM_ (pkgInfo ^. packagePackage . packageDependencies) addDependency'

topModulePathToRelativeFilePath :: String -> String -> (FilePath -> FilePath -> FilePath) -> TopModulePath -> FilePath
topModulePathToRelativeFilePath ext suffix joinpath mp = relFilePath
  where
    relDirPath :: FilePath
    relDirPath = foldr (joinpath . toPath) mempty (mp ^. modulePathDir)
    relFilePath :: FilePath
    relFilePath = addExt (relDirPath `joinpath'` toPath (mp ^. modulePathName) <> suffix)
    joinpath' :: FilePath -> FilePath -> FilePath
    joinpath' l r
      | null l = r
      | otherwise = joinpath l r
    addExt = (<.> ext)
    toPath :: Symbol -> FilePath
    toPath s = unpack (s ^. symbolText)

resolvePath' :: Members '[State ResolverState, Reader ResolverEnv] r => TopModulePath -> Sem r (Either FilesError FilePath)
resolvePath' mp = do
  let rel = topModulePathToRelativeFilePath ".juvix" "" (</>) mp
  m <- gets (^. stateFiles . at rel)
  case m of
    Just (r :| rs) -> case rs of
      [] -> return (Right (r ^. packageRoot </> rel))
      _ -> return (Left (error "file conflict"))
    Nothing -> error ("file does not exist: " <> pack rel)

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
        x :: Either FilesError FilePath <- resolvePath' m
        root <- asks (^. envRoot)
        x' <- pureT x
        a' <- bindT a
        raise (runPathResolver root (a' x'))

runPathResolver :: Members '[Files, Error Text] r => FilePath -> Sem (PathResolver ': r) a -> Sem r a
runPathResolver root = evalState iniResolverState . runReader (ResolverEnv root) . re

runPathResolverPipe :: Members '[Files, Reader EntryPoint] r => Sem (PathResolver ': r) a -> Sem r a
runPathResolverPipe a = do
  r <- asks (^. entryPointRoot)
  runError (runPathResolver r (raiseUnder a)) >>= either error return

-- stdlibOrFile ::
--   forall r.
--   Members '[Embed IO, Error FilesError] r =>
--   FilePath ->
--   FilePath ->
--   Maybe StdlibState ->
--   Sem r FilePath
-- stdlibOrFile p rootPath m = case m of
--   Nothing -> return (rootPath </> p)
--   Just s
--     | HashSet.member (normalise p) (s ^. stdlibFilePaths) ->
--         ifM
--           isConflict
--           ( throw
--               FilesError
--                 { _filesErrorPath = pAbsPath,
--                   _filesErrorCause = StdlibConflict
--                 }
--           )
--           (return (s ^. stdlibRoot </> p))
--     | otherwise -> return pAbsPath
--     where
--       pAbsPath :: FilePath
--       pAbsPath = rootPath </> p

--       isConflict :: Sem r Bool
--       isConflict = do
--         cRootPath <- embed (canonicalizePath rootPath)
--         cStdlibPath <- embed (canonicalizePath (s ^. stdlibRoot))
--         andM [return (cRootPath /= cStdlibPath), embed (doesFileExist pAbsPath)]
