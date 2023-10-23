module Juvix.Compiler.Pipeline.Package
  ( module Juvix.Compiler.Pipeline.Package.Base,
    readPackage,
    readPackageIO,
    readGlobalPackageIO,
    readGlobalPackage,
  )
where

import Data.ByteString qualified as ByteString
import Data.HashSet qualified as HashSet
import Data.Versions
import Data.Yaml
import Juvix.Compiler.Pipeline.Lockfile
import Juvix.Compiler.Pipeline.Package.Base
import Juvix.Compiler.Pipeline.Package.Loader.Error
import Juvix.Extra.Paths
import Juvix.Prelude

processPackage :: forall r. (Members '[Error PackageLoaderError] r) => Path Abs File -> BuildDir -> Maybe LockfileInfo -> RawPackage -> Sem r Package
processPackage _packageFile buildDir lockfile pkg = do
  let _packageName = fromMaybe defaultPackageName (pkg ^. packageName)
      _packageDependencies = resolveDependencies
  checkNoDuplicateDepNames _packageDependencies
  _packageVersion <- getVersion
  return
    Package
      { _packageBuildDir = pkg ^. packageBuildDir,
        _packageMain = pkg ^. packageMain,
        _packageLockfile = lockfile,
        ..
      }
  where
    getVersion :: Sem r SemVer
    getVersion = case pkg ^. packageVersion of
      Nothing -> return defaultVersion
      Just ver -> case semver ver of
        Right v -> return v
        Left err ->
          throw
            PackageLoaderError
              { _packageLoaderErrorCause =
                  ErrVersionParseError
                    VersionParseError {_versionParseErrorError = (pack (errorBundlePretty err))},
                _packageLoaderErrorPath = _packageFile
              }

    checkNoDuplicateDepNames :: [Dependency] -> Sem r ()
    checkNoDuplicateDepNames deps = go HashSet.empty (deps ^.. traversed . _GitDependency . gitDependencyName)
      where
        go :: HashSet Text -> [Text] -> Sem r ()
        go _ [] = return ()
        go s (x : xs)
          | x `HashSet.member` s =
              throw
                PackageLoaderError
                  { _packageLoaderErrorPath = _packageFile,
                    _packageLoaderErrorCause =
                      ErrDuplicateDependencyError
                        DuplicateDependencyError
                          { _duplicateDependencyErrorName = x
                          }
                  }
          | otherwise = go (HashSet.insert x s) xs

    resolveDependencies :: [Dependency]
    resolveDependencies = fromMaybe [stdlib] (pkg ^. packageDependencies)
      where
        base :: SomeBase Dir = resolveBuildDir buildDir <///> relStdlibDir
        stdlib = mkPathDependency (fromSomeDir base)

-- | Given some directory d it tries to read the file d/juvix.yaml and parse its contents
readPackage ::
  forall r.
  (Members '[Files, Error JuvixError] r) =>
  Path Abs Dir ->
  BuildDir ->
  Sem r Package
readPackage root buildDir = mapError (JuvixError @PackageLoaderError) $ do
  bs <- readFileBS' yamlPath
  mLockfile <- mayReadLockfile root
  if
      | ByteString.null bs -> return (emptyPackage buildDir yamlPath)
      | otherwise -> either (throwErr . pack . prettyPrintParseException) (processPackage yamlPath buildDir mLockfile) (decodeEither' bs)
  where
    yamlPath = mkPackageFilePath root

    throwErr e =
      throw
        PackageLoaderError
          { _packageLoaderErrorPath = yamlPath,
            _packageLoaderErrorCause =
              ErrPackageYamlParseError
                PackageYamlParseError
                  { _packageYamlParseErrorError = e
                  }
          }

readPackageIO :: Path Abs Dir -> BuildDir -> IO Package
readPackageIO root buildDir = runM (runFilesIO (runErrorIO' @JuvixError (readPackage root buildDir)))

readGlobalPackageIO :: IO Package
readGlobalPackageIO = runM (runFilesIO . runErrorIO' @JuvixError $ readGlobalPackage)

readGlobalPackage :: (Members '[Error JuvixError, Files] r) => Sem r Package
readGlobalPackage = do
  yamlPath <- globalYaml
  unlessM (fileExists' yamlPath) writeGlobalPackage
  readPackage (parent yamlPath) DefaultBuildDir

writeGlobalPackage :: (Members '[Files] r) => Sem r ()
writeGlobalPackage = do
  yamlPath <- globalYaml
  ensureDir' (parent yamlPath)
  writeFileBS yamlPath (encode globalPackage)
