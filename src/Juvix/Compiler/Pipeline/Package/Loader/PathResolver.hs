module Juvix.Compiler.Pipeline.Package.Loader.PathResolver where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Concrete hiding (Symbol)
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Base
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Data
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Error
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Paths
import Juvix.Compiler.Core.Language
import Juvix.Data.Effect.TaggedLock
import Juvix.Extra.PackageFiles
import Juvix.Extra.Paths
import Juvix.Extra.Stdlib

data RootInfoDirs = RootInfoDirs
  { _rootInfoArgPackageDir :: Path Abs Dir,
    _rootInfoArgPackageBaseDir :: Path Abs Dir,
    _rootInfoArgGlobalStdlibDir :: Path Abs Dir
  }

data RootInfoFiles = RootInfoFiles
  { _rootInfoFilesPackage :: HashSet (Path Rel File),
    _rootInfoFilesPackageBase :: HashSet (Path Rel File)
  }

makeLenses ''RootInfoDirs
makeLenses ''RootInfoFiles

-- | A PackageResolver interpreter intended to be used to load a Package file.
-- It aggregates files at `rootPath` and files from the global package stdlib.
runPackagePathResolver :: forall r a. (Members '[TaggedLock, Files] r) => Path Abs Dir -> Sem (PathResolver ': r) a -> Sem r a
runPackagePathResolver rootPath sem = do
  ds <- rootInfoDirs
  initFiles ds
  fs <- rootInfoFiles ds
  let mkRootInfo' = mkRootInfo ds fs
  ( interpretH $ \case
      RegisterDependencies {} -> pureT ()
      ExpectedPathInfoTopModule m -> do
        let _pathInfoTopModule = m
            _pathInfoRootInfo =
              --  A Package file is a member of a package by definition.
              fromMaybe (error "runPackagePathResolver: expected root info") $
                mkRootInfo' (topModulePathToRelativePath' m)
        pureT PathInfoTopModule {..}
      WithPath m a -> do
        let relPath = topModulePathToRelativePath' m
            x :: Either PathResolverError (Path Abs Dir, Path Rel File)
            x = case mkRootInfo' relPath of
              Just p -> Right (p ^. rootInfoPath, relPath)
              Nothing -> Left (ErrPackageInvalidImport PackageInvalidImport {_packageInvalidImport = m})
        runTSimple (return x) >>= bindTSimple a
    )
    sem
  where
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
    mkRootInfo ds fs relPath
      | parent preludePath `isProperPrefixOf` relPath = mkInfo (ds ^. rootInfoArgGlobalStdlibDir)
      | relPath == packageFilePath = mkInfo rootPath
      | relPath `HashSet.member` (fs ^. rootInfoFilesPackage) = mkInfo (ds ^. rootInfoArgPackageDir)
      | relPath `HashSet.member` (fs ^. rootInfoFilesPackageBase) = mkInfo (ds ^. rootInfoArgPackageBaseDir)
      | otherwise = Nothing
      where
        mkInfo :: Path Abs Dir -> Maybe RootInfo
        mkInfo d =
          Just $
            RootInfo
              { _rootInfoPath = d,
                _rootInfoKind = RootKindPackage
              }

runPackagePathResolver' :: (Members '[TaggedLock, Files] r) => Path Abs Dir -> Sem (PathResolver ': r) a -> Sem r (ResolverState, a)
runPackagePathResolver' root eff = do
  res <- runPackagePathResolver root eff
  return (iniResolverState, res)

runPackagePathResolver'' :: (Members '[TaggedLock, Files] r) => Path Abs Dir -> ResolverState -> Sem (PathResolver ': r) a -> Sem r (ResolverState, a)
runPackagePathResolver'' root _ eff = runPackagePathResolver' root eff
