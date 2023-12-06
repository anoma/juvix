module Juvix.Compiler.Pipeline.Package
  ( module Juvix.Compiler.Pipeline.Package.Base,
    readPackage,
    readPackageIO,
    readPackageRootIO,
    readGlobalPackageIO,
    readGlobalPackage,
    loadPackageFileIO,
    packageBasePackage,
    ensureGlobalPackage,
  )
where

import Data.ByteString qualified as ByteString
import Data.HashSet qualified as HashSet
import Data.Versions
import Data.Yaml
import Juvix.Compiler.Pipeline.Lockfile
import Juvix.Compiler.Pipeline.Package.Base
import Juvix.Compiler.Pipeline.Package.Loader
import Juvix.Compiler.Pipeline.Package.Loader.Error
import Juvix.Compiler.Pipeline.Package.Loader.EvalEff
import Juvix.Compiler.Pipeline.Package.Loader.EvalEff.IO
import Juvix.Compiler.Pipeline.Root.Base
import Juvix.Compiler.Pipeline.Root.Base qualified as Root
import Juvix.Data.Effect.TaggedLock
import Juvix.Extra.Paths
import Juvix.Prelude

processPackage :: forall r. (Members '[Error PackageLoaderError] r) => Path Abs File -> BuildDir -> Maybe LockfileInfo -> RawPackage -> Sem r Package
processPackage _packageFile buildDir lockfile pkg = do
  let _packageName = fromMaybe defaultPackageName (pkg ^. packageName)
      _packageDependencies = resolveDependencies
  checkNoDuplicateDepNames _packageFile _packageDependencies
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

    resolveDependencies :: [Dependency]
    resolveDependencies = fromMaybe [stdlib] (pkg ^. packageDependencies)
      where
        base :: SomeBase Dir = resolveBuildDir buildDir <///> relStdlibDir
        stdlib = mkPathDependency (fromSomeDir base)

checkNoDuplicateDepNames :: forall r. (Members '[Error PackageLoaderError] r) => Path Abs File -> [Dependency] -> Sem r ()
checkNoDuplicateDepNames p deps = go HashSet.empty (deps ^.. traversed . _GitDependency . gitDependencyName)
  where
    go :: HashSet Text -> [Text] -> Sem r ()
    go _ [] = return ()
    go s (x : xs)
      | x `HashSet.member` s =
          throw
            PackageLoaderError
              { _packageLoaderErrorPath = p,
                _packageLoaderErrorCause =
                  ErrDuplicateDependencyError
                    DuplicateDependencyError
                      { _duplicateDependencyErrorName = x
                      }
              }
      | otherwise = go (HashSet.insert x s) xs

readPackage ::
  forall r.
  (Members '[Error JuvixError, Files, EvalFileEff] r) =>
  Path Abs Dir ->
  BuildDir ->
  Sem r Package
readPackage root buildDir = do
  ifM (fileExists' f) (readPackageFile root buildDir f) (readYamlPackage root buildDir)
  where
    f :: Path Abs File
    f = mkPackagePath root

-- | Given some directory d it tries to read the file d/juvix.yaml and parse its contents
readYamlPackage ::
  forall r.
  (Members '[Files, EvalFileEff, Error JuvixError] r) =>
  Path Abs Dir ->
  BuildDir ->
  Sem r Package
readYamlPackage root buildDir = mapError (JuvixError @PackageLoaderError) $ do
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

readPackageFile ::
  (Members '[Files, Error JuvixError, EvalFileEff] r) =>
  Path Abs Dir ->
  BuildDir ->
  Path Abs File ->
  Sem r Package
readPackageFile root buildDir f = mapError (JuvixError @PackageLoaderError) $ do
  pkg <- loadPackage buildDir f
  mLockfile <- mayReadLockfile root
  checkNoDuplicateDepNames f (pkg ^. packageDependencies)
  return (pkg {_packageLockfile = mLockfile})

loadPackageFileIO :: (Members '[TaggedLock, Error JuvixError, Embed IO] r) => Path Abs Dir -> BuildDir -> Sem r Package
loadPackageFileIO root buildDir =
  runFilesIO
    . mapError (JuvixError @PackageLoaderError)
    . runEvalFileEffIO
    $ loadPackage buildDir (mkPackagePath root)

readPackageRootIO :: (Members '[TaggedLock, Embed IO] r) => Root -> Sem r Package
readPackageRootIO root = readPackageIO (root ^. rootRootDir) (root ^. Root.rootBuildDir)

readPackageIO :: (Members '[TaggedLock, Embed IO] r) => Path Abs Dir -> BuildDir -> Sem r Package
readPackageIO root buildDir =
  runFilesIO
    . runErrorIO' @JuvixError
    . mapError (JuvixError @PackageLoaderError)
    . runEvalFileEffIO
    $ readPackage root buildDir

readGlobalPackageIO :: (Members '[Embed IO, TaggedLock] r) => Sem r Package
readGlobalPackageIO =
  runFilesIO
    . runErrorIO' @JuvixError
    . mapError (JuvixError @PackageLoaderError)
    . runEvalFileEffIO
    $ readGlobalPackage

ensureGlobalPackage :: (Members '[TaggedLock, Files] r) => Sem r (Path Abs File)
ensureGlobalPackage = do
  packagePath <- globalPackageJuvix
  withTaggedLockDir (parent packagePath) (unlessM (fileExists' packagePath) writeGlobalPackage)
  return packagePath

readGlobalPackage :: (Members '[TaggedLock, Error JuvixError, EvalFileEff, Files] r) => Sem r Package
readGlobalPackage = do
  packagePath <- ensureGlobalPackage
  readPackage (parent packagePath) DefaultBuildDir

writeGlobalPackage :: (Members '[Files] r) => Sem r ()
writeGlobalPackage = do
  packagePath <- globalPackageJuvix
  ensureDir' (parent packagePath)
  writeFile' packagePath (renderPackageVersion currentPackageVersion (globalPackage packagePath))

packageBasePackage :: Package
packageBasePackage =
  Package
    { _packageVersion = defaultVersion,
      _packageName = "package-base",
      _packageMain = Nothing,
      _packageLockfile = Nothing,
      _packageFile = $(mkAbsFile "/<package-base>"),
      _packageDependencies = [],
      _packageBuildDir = Nothing
    }
