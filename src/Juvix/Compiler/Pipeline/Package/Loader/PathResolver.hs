module Juvix.Compiler.Pipeline.Package.Loader.PathResolver where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Concrete hiding (Symbol)
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Loader.PathResolver
import Juvix.Compiler.Pipeline.Package.Loader.EvalEff
import Juvix.Extra.PackageFiles
import Juvix.Extra.Paths
import Juvix.Extra.Stdlib

data RootInfoDirs = RootInfoDirs
  { _rootInfoArgPackageDir :: Path Abs Dir,
    _rootInfoArgPackageBaseDir :: Path Abs Dir,
    _rootInfoArgGlobalStdlibDir :: Path Abs Dir
  }
  deriving stock (Show)

data RootInfoFiles = RootInfoFiles
  { _rootInfoFilesPackage :: HashSet (Path Rel File),
    _rootInfoFilesPackageBase :: HashSet (Path Rel File)
  }

makeLenses ''RootInfoDirs
makeLenses ''RootInfoFiles

-- | A PackageResolver interpreter intended to be used to load a Package file.
-- It aggregates the package.juvix file in `rootPath` and the package-base,
-- package and global standard library (currently under global-package/.juvix-build)
runPackagePathResolver ::
  forall r a.
  (Members '[TaggedLock, Error JuvixError, Files, EvalFileEff, Reader EntryPoint] r) =>
  Path Abs Dir ->
  Sem (PathResolver ': r) a ->
  Sem r a
runPackagePathResolver rootPath sem = do
  ds <- rootInfoDirs
  initFiles ds
  fs <- rootInfoFiles ds
  let mkRootInfo' :: Path Rel File -> Maybe RootInfo = mkRootInfo ds fs
  packageInfos <- mkPackageInfos ds fs
  (`interpretH` sem) $ \localEnv -> \case
    SupportsParallel -> return False
    ResolverRoot -> return rootPath
    ResolverInitialRoot -> return rootPath
    RegisterDependencies {} -> return ()
    ResolvePath scan -> case mkRootInfo ds fs (addFileExt FileExtJuvix (importScanToRelPath scan)) of
      Nothing ->
        throw . JuvixError $
          ErrPackageInvalidImport
            PackageInvalidImport
              { _packageInvalidImport = scan
              }
      Just (ri :: RootInfo) ->
        let pkg = packageInfos ^?! at (ri ^. rootInfoPath) . _Just
         in return (pkg, FileExtJuvix)
    GetPackageInfos -> return packageInfos
    ExpectedPathInfoTopModule m -> do
      let _pathInfoTopModule = m
          _pathInfoRootInfo =
            --  A Package file is a member of a package by definition.
            fromMaybe (error "runPackagePathResolver: expected root info") $
              mkRootInfo' (topModulePathToRelativePath' m)
      return PathInfoTopModule {..}
    WithResolverRoot _root' m ->
      -- the _root' is not used because ResolvePath does not depend on it
      runTSimpleEff localEnv m
  where
    mkPackageInfos ::
      RootInfoDirs ->
      RootInfoFiles ->
      Sem r (HashMap (Path Abs Dir) PackageInfo)
    mkPackageInfos ds fs = do
      pkgBase <- mkPkgBase
      gstdlib <- mkPkgGlobalStdlib
      pkgDotJuvix <- mkPackageDotJuvix
      pkgType <- mkPkgPackageType
      return
        . hashMap
        $ mkAssoc <$> [pkgBase, pkgType, gstdlib, pkgDotJuvix]
      where
        mkAssoc :: PackageInfo -> (Path Abs Dir, PackageInfo)
        mkAssoc pkg = (pkg ^. packageRoot, pkg)

        mkPkgBase :: Sem r PackageInfo
        mkPkgBase = do
          let rfiles = fs ^. rootInfoFilesPackageBase
          return
            PackageInfo
              { _packageRoot = ds ^. rootInfoArgPackageBaseDir,
                _packageAvailableRoots = hashSet [ds ^. rootInfoArgPackageBaseDir],
                _packageJuvixRelativeFiles = rfiles,
                _packagePackage = PackageBase
              }

        mkPkgPackageType :: Sem r PackageInfo
        mkPkgPackageType = do
          let rfiles = fs ^. rootInfoFilesPackage
              root = ds ^. rootInfoArgPackageDir
          return
            PackageInfo
              { _packageRoot = root,
                _packageJuvixRelativeFiles = rfiles,
                _packagePackage = PackageType,
                _packageAvailableRoots =
                  hashSet
                    [ ds ^. rootInfoArgPackageDir,
                      ds ^. rootInfoArgPackageBaseDir,
                      ds ^. rootInfoArgGlobalStdlibDir
                    ]
              }

        mkPkgGlobalStdlib :: Sem r PackageInfo
        mkPkgGlobalStdlib = do
          let root = ds ^. rootInfoArgGlobalStdlibDir
          jufiles <- findPackageJuvixFiles root
          let rfiles = hashSet jufiles
          return
            PackageInfo
              { _packageRoot = root,
                _packageJuvixRelativeFiles = rfiles,
                _packageAvailableRoots =
                  hashSet
                    [ ds ^. rootInfoArgPackageBaseDir,
                      ds ^. rootInfoArgGlobalStdlibDir
                    ],
                _packagePackage = PackageGlobalStdlib
              }

        mkPackageDotJuvix :: Sem r PackageInfo
        mkPackageDotJuvix = do
          let rfiles = hashSet [packageFilePath]
          return
            PackageInfo
              { _packageRoot = rootPath,
                _packageJuvixRelativeFiles = rfiles,
                _packageAvailableRoots =
                  hashSet
                    [ ds ^. rootInfoArgPackageBaseDir,
                      ds ^. rootInfoArgPackageDir,
                      ds ^. rootInfoArgGlobalStdlibDir,
                      rootPath
                    ],
                _packagePackage = PackageDotJuvix
              }

    rootInfoDirs :: Sem r RootInfoDirs
    rootInfoDirs = do
      _rootInfoArgGlobalStdlibDir <- juvixStdlibDir . rootBuildDir <$> globalRoot
      _rootInfoArgPackageDir <- globalPackageDescriptionRoot
      _rootInfoArgPackageBaseDir <- globalPackageBaseRoot
      return RootInfoDirs {..}

    initFiles :: RootInfoDirs -> Sem r ()
    initFiles ds = do
      runReader (ds ^. rootInfoArgGlobalStdlibDir) updateStdlib
      runReader (ds ^. rootInfoArgPackageDir) updatePackageFiles
      runReader (ds ^. rootInfoArgPackageBaseDir) updatePackageBaseFiles

    rootInfoFiles :: RootInfoDirs -> Sem r RootInfoFiles
    rootInfoFiles ds = do
      _rootInfoFilesPackage <- relFiles (ds ^. rootInfoArgPackageDir)
      _rootInfoFilesPackageBase <- relFiles (ds ^. rootInfoArgPackageBaseDir)
      return RootInfoFiles {..}

    mkRootInfo :: RootInfoDirs -> RootInfoFiles -> Path Rel File -> Maybe RootInfo
    mkRootInfo ds fs relPath = mkInfo <$> mrootInfoPath
      where
        mrootInfoPath :: Maybe (Path Abs Dir)
        mrootInfoPath
          | parent preludePath `isProperPrefixOf` relPath = Just (ds ^. rootInfoArgGlobalStdlibDir)
          | relPath == packageFilePath = Just rootPath
          | relPath `HashSet.member` (fs ^. rootInfoFilesPackage) = Just (ds ^. rootInfoArgPackageDir)
          | relPath `HashSet.member` (fs ^. rootInfoFilesPackageBase) = Just (ds ^. rootInfoArgPackageBaseDir)
          | otherwise = Nothing
        mkInfo :: Path Abs Dir -> RootInfo
        mkInfo d =
          RootInfo
            { _rootInfoPath = d,
              _rootInfoKind = RootKindPackage
            }

runPackagePathResolver' ::
  (Members '[TaggedLock, Error JuvixError, Files, EvalFileEff, Reader EntryPoint] r) =>
  Path Abs Dir ->
  Sem (PathResolver ': r) a ->
  Sem r (ResolverState, a)
runPackagePathResolver' root eff = do
  res <- runPackagePathResolver root eff
  return (iniResolverState, res)

runPackagePathResolver'' :: (Members '[Reader EntryPoint, TaggedLock, Error JuvixError, Files, EvalFileEff] r) => Path Abs Dir -> ResolverState -> Sem (PathResolver ': r) a -> Sem r (ResolverState, a)
runPackagePathResolver'' root _ eff = runPackagePathResolver' root eff
