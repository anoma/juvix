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
  let mkPackageRoot' = mkPackageRoot packageFiles' globalPackageDir globalStdlib
  ( interpretH $ \case
      RegisterDependencies {} -> pureT ()
      ExpectedModulePath t m -> do
        let _pathInfoRelPath = topModulePathToRelativePath' m
            _pathInfoTopModule = m ^. topModulePath
            _pathInfoPackageRoot = mkPackageRoot' _pathInfoRelPath
        pureT . Just $ PathInfoTopModule {..}
      WithPath m a -> do
        let relPath = topModulePathToRelativePath' m
            x :: Either PathResolverError (Path Abs Dir, Path Rel File)
            x = case mkPackageRoot' relPath of
              Just p -> Right (p ^. packageRoot, relPath)
              Nothing -> Left (ErrPackageInvalidImport PackageInvalidImport {_packageInvalidImport = m})
        runTSimple (return x) >>= bindTSimple a
    )
    sem
  where
    mkPackageRoot :: HashSet (Path Rel File) -> Path Abs Dir -> Path Abs Dir -> Path Rel File -> Maybe PackageRoot
    mkPackageRoot pkgFiles globalPackageDir globalStdlib relPath
      | parent preludePath `isProperPrefixOf` relPath =
        Just $ PackageRoot {
          _packageRoot = globalStdlib,
          _packageRootType = RootGlobalStdlib
        }
      | relPath `HashSet.member` pkgFiles =
        Just $ PackageRoot {
          _packageRoot = globalPackageDir,
          _packageRootType = RootGlobalPackage
        }
      | relPath == packageFilePath = 
        Just $ PackageRoot {
          _packageRoot = rootPath,
          _packageRootType = RootLocalPackage
        }
      | otherwise = Nothing

runPackagePathResolver' :: (Members '[TaggedLock, Files] r) => Path Abs Dir -> Sem (PathResolver ': r) a -> Sem r (ResolverState, a)
runPackagePathResolver' root eff = do
  res <- runPackagePathResolver root eff
  return (iniResolverState, res)

runPackagePathResolver'' :: (Members '[TaggedLock, Files] r) => Path Abs Dir -> ResolverState -> Sem (PathResolver ': r) a -> Sem r (ResolverState, a)
runPackagePathResolver'' root _ eff = runPackagePathResolver' root eff
