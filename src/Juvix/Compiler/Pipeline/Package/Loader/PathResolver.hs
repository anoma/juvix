module Juvix.Compiler.Pipeline.Package.Loader.PathResolver where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Concrete hiding (Symbol)
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Pipeline.Loader.PathResolver
import Juvix.Compiler.Pipeline.Package (packageBasePackage, packageJuvixPackage)
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
  (Members '[TaggedLock, Error JuvixError, Files] r) =>
  Path Abs Dir ->
  Sem (PathResolver ': r) a ->
  Sem r a
runPackagePathResolver rootPath sem = do
  ds <- rootInfoDirs
  initFiles ds
  fs <- rootInfoFiles ds
  let mkRootInfo' :: Path Rel File -> Maybe RootInfo = mkRootInfo ds fs
      packageInfos = mkPackageInfos ds fs
  (`interpretH` sem) $ \localEnv -> \case
    RegisterDependencies {} -> return ()
    ResolvePath scan -> case mkRootInfo ds fs (addFileExt FileExtJuvix (importScanToRelPath scan)) of
      Nothing ->
        throw . JuvixError $
          ErrPackageInvalidImport
            PackageInvalidImport
              { _packageInvalidImport = undefined -- TODO put a path
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
    mkPackageInfos :: RootInfoDirs -> RootInfoFiles -> HashMap (Path Abs Dir) PackageInfo
    mkPackageInfos ds fs =
      hashMap
        [ -- package-base
          ( ds ^. rootInfoArgPackageBaseDir,
            PackageInfo
              { _packageRoot = ds ^. rootInfoArgPackageBaseDir,
                _packageAvailableRoots = hashSet [ds ^. rootInfoArgPackageBaseDir],
                _packageRelativeFiles = fs ^. rootInfoFilesPackageBase,
                _packageImports = error "TODO: scan _packageRelativeFiles",
                _packagePackage = packageBasePackage
              }
          ),
          ( ds ^. rootInfoArgPackageDir,
            PackageInfo
              { _packageRoot = ds ^. rootInfoArgPackageDir,
                _packageRelativeFiles = fs ^. rootInfoFilesPackage,
                _packageImports = error "TODO: scan _packageRelativeFiles files",
                _packagePackage = error "TODO: read it from _packageRelativeFiles?",
                _packageAvailableRoots =
                  hashSet
                    [ ds ^. rootInfoArgPackageDir,
                      ds ^. rootInfoArgPackageBaseDir,
                      ds ^. rootInfoArgGlobalStdlibDir
                    ]
              }
          ),
          ( ds ^. rootInfoArgGlobalStdlibDir,
            PackageInfo
              { _packageRoot = ds ^. rootInfoArgGlobalStdlibDir,
                _packageRelativeFiles = error "TODO: compute from stdlibFiles",
                _packageImports = error "TODO: compute from stdlibFiles",
                _packageAvailableRoots =
                  hashSet
                    [ ds ^. rootInfoArgPackageBaseDir,
                      ds ^. rootInfoArgGlobalStdlibDir
                    ],
                _packagePackage = error "TODO: read it from stdlibFiles"
              }
          ),
          ( rootPath,
            PackageInfo
              { _packageRoot = rootPath,
                _packageRelativeFiles = hashSet [packageFilePath],
                _packageImports = hashMap [(packageFilePath, error "TODO: scan actual file")],
                _packageAvailableRoots =
                  hashSet
                    [ ds ^. rootInfoArgPackageBaseDir,
                      ds ^. rootInfoArgPackageDir,
                      ds ^. rootInfoArgGlobalStdlibDir,
                      rootPath
                    ],
                _packagePackage = packageJuvixPackage
              }
          )
        ]

    -- WithPath m a -> do
    --   let relPath = topModulePathToRelativePath' m
    --       x :: Either PathResolverError (Path Abs Dir, Path Rel File)
    --       x = case mkRootInfo' relPath of
    --         Just p -> Right (p ^. rootInfoPath, relPath)
    --         Nothing -> Left (ErrPackageInvalidImport PackageInvalidImport {_packageInvalidImport = m})
    -- runTSimpleEff localEnv (a x)

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
  (Members '[TaggedLock, Error JuvixError, Files] r) =>
  Path Abs Dir ->
  Sem (PathResolver ': r) a ->
  Sem r (ResolverState, a)
runPackagePathResolver' root eff = do
  res <- runPackagePathResolver root eff
  return (iniResolverState, res)

runPackagePathResolver'' :: (Members '[TaggedLock, Error JuvixError, Files] r) => Path Abs Dir -> ResolverState -> Sem (PathResolver ': r) a -> Sem r (ResolverState, a)
runPackagePathResolver'' root _ eff = runPackagePathResolver' root eff
