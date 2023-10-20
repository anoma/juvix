module Juvix.Compiler.Pipeline.Package.Loader.PathResolver where

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
        pureT ((<//> relPath) <$> (mkPackageRoot' relPath))
      WithPath m a -> do
        let relPath = topModulePathToRelativePath' m
            x :: Either PathResolverError (Path Abs Dir, Path Rel File)
            x = case mkPackageRoot' relPath of
              Just p -> Right (p, relPath)
              Nothing -> Left (ErrPackageInvalidImport PackageInvalidImport {_packageInvalidImport = m})
        runTSimple (return x) >>= bindTSimple a
    )
    sem
  where
    mkPackageRoot :: HashSet (Path Rel File) -> Path Abs Dir -> Path Abs Dir -> Path Rel File -> Maybe (Path Abs Dir)
    mkPackageRoot pkgFiles globalPackageDir globalStdlib relPath
      | parent preludePath `isProperPrefixOf` relPath = Just globalStdlib
      | relPath `HashSet.member` pkgFiles = Just globalPackageDir
      | relPath == packageFilePath = Just rootPath
      | otherwise = Nothing

runPackagePathResolver' :: (Members '[Files] r) => Path Abs Dir -> Sem (PathResolver ': r) a -> Sem r (ResolverState, a)
runPackagePathResolver' root eff = do
  res <- runPackagePathResolver root eff
  return (iniResolverState, res)
