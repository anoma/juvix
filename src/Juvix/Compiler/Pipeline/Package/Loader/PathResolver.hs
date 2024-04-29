module Juvix.Compiler.Pipeline.Package.Loader.PathResolver where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Concrete hiding (Symbol)
import Juvix.Compiler.Concrete.Translation.ImportScanner.FlatParse
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Loader.PathResolver
import Juvix.Compiler.Pipeline.Package (packageBasePackage, packageJuvixPackage, packagePackageStdlib)
import Juvix.Compiler.Pipeline.Package.Loader.EvalEff
import Juvix.Data.Effect.TaggedLock
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
    scanHelperFiles :: Path Abs Dir -> HashSet (Path Rel File) -> Sem r (HashMap (Path Rel File) (HashSet ImportScan))
    scanHelperFiles pkgRoot relPkgFiles =
      let getScans :: Path Rel File -> Sem r (HashSet ImportScan)
          getScans p = scanFileImports (pkgRoot <//> p)
       in hashMapFromHashSetM getScans relPkgFiles

    scanHelperPure :: Path Abs Dir -> HashSet (Path Rel File) -> [(Path Rel File, ByteString)] -> HashMap (Path Rel File) (HashSet ImportScan)
    scanHelperPure pkgRoot relPkgFiles fileContents =
      let getScans :: Path Rel File -> HashSet ImportScan
          getScans p = scanBSImports (pkgRoot <//> p) (fileContentsMap ^?! at p . _Just)
          fileContentsMap = hashMap fileContents
       in hashMapFromHashSet getScans relPkgFiles

    mkPackageInfos ::
      RootInfoDirs ->
      RootInfoFiles ->
      Sem r (HashMap (Path Abs Dir) PackageInfo)
    mkPackageInfos ds fs = do
      gstdlib <- mkPkgGlobalStdlib
      pkgDotJuvix <- mkPackageDotJuvix
      pkgType <- mkPkgPackageType
      return
        . hashMap
        $ mkAssoc <$> [pkgBase, pkgType, gstdlib, pkgDotJuvix]
      where
        mkAssoc :: PackageInfo -> (Path Abs Dir, PackageInfo)
        mkAssoc pkg = (pkg ^. packageRoot, pkg)

        pkgBase :: PackageInfo
        pkgBase =
          let rfiles = fs ^. rootInfoFilesPackageBase
              imports = scanHelperPure (ds ^. rootInfoArgPackageBaseDir) rfiles packageBaseFiles
           in PackageInfo
                { _packageRoot = ds ^. rootInfoArgPackageBaseDir,
                  _packageAvailableRoots = hashSet [ds ^. rootInfoArgPackageBaseDir],
                  _packageRelativeFiles = rfiles,
                  _packageImports = imports,
                  _packagePackage = packageBasePackage
                }

        mkPkgPackageType :: Sem r PackageInfo
        mkPkgPackageType = do
          let rfiles = HashSet.filter (/= packageFilePath) (fs ^. rootInfoFilesPackage)
              imports = scanHelperPure (ds ^. rootInfoArgPackageDir) rfiles packagePackageFiles
              root = ds ^. rootInfoArgPackageDir
          let pkg :: Package = packageJuvixPackage
          return
            PackageInfo
              { _packageRoot = root,
                _packageRelativeFiles = rfiles,
                _packageImports = imports,
                _packagePackage = pkg,
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
          jufiles <- findJuvixFiles root
          let rfiles = hashSet (filter (/= packageFilePath) jufiles)
              pkg :: Package = packagePackageStdlib
          imports <- scanHelperFiles root rfiles
          return
            PackageInfo
              { _packageRoot = root,
                _packageRelativeFiles = rfiles,
                _packageImports = imports,
                _packageAvailableRoots =
                  hashSet
                    [ ds ^. rootInfoArgPackageBaseDir,
                      ds ^. rootInfoArgGlobalStdlibDir
                    ],
                _packagePackage = pkg
              }

        mkPackageDotJuvix :: Sem r PackageInfo
        mkPackageDotJuvix = do
          -- buildDir <- asks (^. entryPointBuildDir)
          -- pkg <- readPackageFile rootPath buildDir (rootPath <//> packageFilePath)
          let pkg :: Package = error "TODO pkg"
          let rfiles = hashSet [packageFilePath]
          imports <- scanHelperFiles rootPath rfiles
          return
            PackageInfo
              { _packageRoot = rootPath,
                _packageRelativeFiles = rfiles,
                _packageImports = imports,
                _packageAvailableRoots =
                  hashSet
                    [ ds ^. rootInfoArgPackageBaseDir,
                      ds ^. rootInfoArgPackageDir,
                      ds ^. rootInfoArgGlobalStdlibDir,
                      rootPath
                    ],
                _packagePackage = pkg
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
