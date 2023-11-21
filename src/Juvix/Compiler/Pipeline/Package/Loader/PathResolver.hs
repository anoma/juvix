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

-- | A PackageResolver interpreter intended to be used to load a Package file.
-- It aggregates files at `rootPath` and files from the global package stdlib.
runPackagePathResolver :: forall r a. (Members '[TaggedLock, Files] r) => Path Abs Dir -> Sem (PathResolver ': r) a -> Sem r a
runPackagePathResolver rootPath sem = do
  globalStdlib <- juvixStdlibDir . rootBuildDir <$> globalRoot
  globalPackageDir <- globalPackageDescriptionRoot
  runReader globalStdlib updateStdlib
  runReader globalPackageDir updatePackageFiles
  packageFiles' <- relFiles globalPackageDir
  let mkRootInfo' = mkRootInfo packageFiles' globalPackageDir globalStdlib
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
    mkRootInfo :: HashSet (Path Rel File) -> Path Abs Dir -> Path Abs Dir -> Path Rel File -> Maybe RootInfo
    mkRootInfo pkgFiles globalPackageDir globalStdlib relPath
      | parent preludePath `isProperPrefixOf` relPath =
          Just $
            RootInfo
              { _rootInfoPath = globalStdlib,
                _rootInfoKind = RootKindPackage
              }
      | relPath == packageFilePath =
          Just $
            RootInfo
              { _rootInfoPath = rootPath,
                _rootInfoKind = RootKindPackage
              }
      | relPath `HashSet.member` pkgFiles =
          Just $
            RootInfo
              { _rootInfoPath = globalPackageDir,
                _rootInfoKind = RootKindPackage
              }
      | otherwise = Nothing

runPackagePathResolver' :: (Members '[TaggedLock, Files] r) => Path Abs Dir -> Sem (PathResolver ': r) a -> Sem r (ResolverState, a)
runPackagePathResolver' root eff = do
  res <- runPackagePathResolver root eff
  return (iniResolverState, res)

runPackagePathResolver'' :: (Members '[TaggedLock, Files] r) => Path Abs Dir -> ResolverState -> Sem (PathResolver ': r) a -> Sem r (ResolverState, a)
runPackagePathResolver'' root _ eff = runPackagePathResolver' root eff
