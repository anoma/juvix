module Juvix.Compiler.Pipeline.Loader.PathResolver where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Concrete hiding (Symbol)
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
import Juvix.Compiler.Core.Language
import Juvix.Extra.PackageFiles
import Juvix.Extra.Paths
import Juvix.Extra.Stdlib

-- | A PackageResolver interpreter intended to be used to load a Package file.
-- It aggregates files at `rootPath` and files from the global package stdlib.
runPackagePathResolver :: forall r a. (Members '[Files] r) => Path Abs Dir -> Sem (PathResolver ': r) a -> Sem r a
runPackagePathResolver rootPath sem = do
  globalStdlib <- juvixStdlibDir . rootBuildDir <$> globalRoot
  globalPackageDir <- globalPackageDescriptionRoot
  runReader globalStdlib updateStdlib
  runReader globalPackageDir updatePackageFiles
  packageFiles' <- relFiles globalPackageDir
  let mkPackageRoot' = mkPackageRoot packageFiles' globalPackageDir globalStdlib
  ( interpretH $ \case
      RegisterDependencies {} -> pureT ()
      ExpectedModulePath _ m -> do
        let relPath = topModulePathToRelativePath' m
        pureT (Just ((mkPackageRoot' relPath) <//> relPath))
      WithPath m a -> do
        let relPath = topModulePathToRelativePath' m
            x :: (Path Abs Dir, Path Rel File)
            x = (mkPackageRoot' relPath, relPath)
        runTSimple (return (Right x)) >>= bindTSimple a
    )
    sem
  where
    mkPackageRoot :: HashSet (Path Rel File) -> Path Abs Dir -> Path Abs Dir -> Path Rel File -> Path Abs Dir
    mkPackageRoot pkgFiles globalPackageDir globalStdlib relPath
      | parent preludePath `isProperPrefixOf` relPath = globalStdlib
      | relPath `HashSet.member` pkgFiles = globalPackageDir
      | otherwise = rootPath

runPackagePathResolver' :: (Members '[Files] r) => Path Abs Dir -> Sem (PathResolver ': r) a -> Sem r (ResolverState, a)
runPackagePathResolver' root eff = do
  res <- runPackagePathResolver root eff
  return (iniResolverState, res)
