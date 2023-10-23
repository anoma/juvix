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
import Juvix.Extra.Paths
import Juvix.Prelude

processPackage :: forall r. (Members '[Error Text] r) => Path Abs File -> BuildDir -> Maybe LockfileInfo -> RawPackage -> Sem r Package
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
        Left err -> throw (pack (errorBundlePretty err))

    checkNoDuplicateDepNames :: [Dependency] -> Sem r ()
    checkNoDuplicateDepNames deps = go HashSet.empty (deps ^.. traversed . _GitDependency . gitDependencyName)
      where
        go :: HashSet Text -> [Text] -> Sem r ()
        go _ [] = return ()
        go s (x : xs)
          | x `HashSet.member` s = throw (errMsg x)
          | otherwise = go (HashSet.insert x s) xs
          where
            errMsg :: Text -> Text
            errMsg dupName =
              "Juvix package file at: "
                <> pack (toFilePath _packageFile)
                <> " contains the duplicate dependency name: "
                <> dupName

    resolveDependencies :: [Dependency]
    resolveDependencies = fromMaybe [stdlib] (pkg ^. packageDependencies)
      where
        base :: SomeBase Dir = resolveBuildDir buildDir <///> relStdlibDir
        stdlib = mkPathDependency (fromSomeDir base)

-- | Given some directory d it tries to read the file d/juvix.yaml and parse its contents
readPackage ::
  forall r.
  (Members '[Files, Error Text] r) =>
  Path Abs Dir ->
  BuildDir ->
  Sem r Package
readPackage root buildDir = do
  bs <- readFileBS' yamlPath
  mLockfile <- mayReadLockfile root
  if
      | ByteString.null bs -> return (emptyPackage buildDir yamlPath)
      | otherwise -> either (throw . pack . prettyPrintParseException) (processPackage yamlPath buildDir mLockfile) (decodeEither' bs)
  where
    yamlPath = mkPackageFilePath root

readPackageIO :: Path Abs Dir -> BuildDir -> IO Package
readPackageIO root buildDir = do
  let x :: Sem '[Error Text, Files, Embed IO] Package
      x = readPackage root buildDir
  m <- runM $ runFilesIO (runError x)
  case m of
    Left err -> putStrLn err >> exitFailure
    Right r -> return r

readGlobalPackageIO :: IO Package
readGlobalPackageIO = do
  m <- runM . runFilesIO . runError $ readGlobalPackage
  case m of
    Left err -> putStrLn err >> exitFailure
    Right r -> return r

readGlobalPackage :: (Members '[Error Text, Files] r) => Sem r Package
readGlobalPackage = do
  yamlPath <- globalYaml
  unlessM (fileExists' yamlPath) writeGlobalPackage
  readPackage (parent yamlPath) DefaultBuildDir

writeGlobalPackage :: (Members '[Files] r) => Sem r ()
writeGlobalPackage = do
  yamlPath <- globalYaml
  ensureDir' (parent yamlPath)
  writeFileBS yamlPath (encode globalPackage)
