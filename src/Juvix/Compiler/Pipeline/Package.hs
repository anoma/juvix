module Juvix.Compiler.Pipeline.Package
  ( module Juvix.Compiler.Pipeline.Package.Dependency,
    BuildDir (..),
    RawPackage,
    Package,
    Package' (..),
    defaultVersion,
    defaultStdlibDep,
    packageName,
    packageBuildDir,
    packageVersion,
    packageDependencies,
    packageMain,
    packageFile,
    rawPackage,
    readPackage,
    readPackageIO,
    readGlobalPackageIO,
    globalPackage,
    emptyPackage,
    readGlobalPackage,
    mkPackageFilePath,
  )
where

import Data.Aeson (genericToEncoding, genericToJSON)
import Data.Aeson.BetterErrors
import Data.Aeson.TH
import Data.ByteString qualified as ByteString
import Data.Kind qualified as GHC
import Data.Set qualified as Set
import Data.Versions
import Data.Yaml
import Juvix.Compiler.Pipeline.Package.Dependency
import Juvix.Extra.Paths
import Juvix.Prelude
import Lens.Micro.Platform qualified as Lens

type NameType :: IsProcessed -> GHC.Type
type family NameType s = res | res -> s where
  NameType 'Raw = Maybe Text
  NameType 'Processed = Text

type VersionType :: IsProcessed -> GHC.Type
type family VersionType s = res | res -> s where
  VersionType 'Raw = Maybe Text
  VersionType 'Processed = SemVer

type DependenciesType :: IsProcessed -> GHC.Type
type family DependenciesType s = res | res -> s where
  DependenciesType 'Raw = Maybe [Dependency]
  DependenciesType 'Processed = [Dependency]

type PackageFileType :: IsProcessed -> GHC.Type
type family PackageFileType s = res | res -> s where
  PackageFileType 'Raw = ()
  PackageFileType 'Processed = Path Abs File

data Package' (s :: IsProcessed) = Package
  { _packageName :: NameType s,
    _packageVersion :: VersionType s,
    _packageDependencies :: DependenciesType s,
    _packageBuildDir :: Maybe (SomeBase Dir),
    _packageMain :: Maybe (Prepath File),
    _packageFile :: PackageFileType s
  }
  deriving stock (Generic)

makeLenses ''Package'

type Package = Package' 'Processed

type RawPackage = Package' 'Raw

deriving stock instance Eq RawPackage

deriving stock instance Eq Package

deriving stock instance Show RawPackage

deriving stock instance Show Package

rawPackageOptions :: Options
rawPackageOptions =
  defaultOptions
    { fieldLabelModifier = over Lens._head toLower . dropPrefix "_package",
      rejectUnknownFields = True,
      omitNothingFields = True
    }

instance ToJSON RawPackage where
  toJSON = genericToJSON rawPackageOptions
  toEncoding = genericToEncoding rawPackageOptions

instance FromJSON RawPackage where
  parseJSON = toAesonParser' (fromMaybe err <$> p)
    where
      p :: Parse' (Maybe RawPackage)
      p = perhaps $ do
        _packageName <- keyMay "name" asText
        _packageVersion <- keyMay "version" asText
        _packageDependencies <- keyMay "dependencies" fromAesonParser
        _packageBuildDir <- keyMay "build-dir" fromAesonParser
        _packageMain <- keyMay "main" fromAesonParser
        let _packageFile = ()
        return Package {..}
      err :: a
      err = error "Failed to parse juvix.yaml"

data BuildDir
  = DefaultBuildDir
  | CustomBuildDir (SomeBase Dir)

resolveBuildDir :: BuildDir -> SomeBase Dir
resolveBuildDir = \case
  DefaultBuildDir -> Rel (relBuildDir)
  CustomBuildDir d -> d

-- | This is used when juvix.yaml exists but it is empty
emptyPackage :: BuildDir -> Path Abs File -> Package
emptyPackage buildDir yamlPath =
  Package
    { _packageName = defaultPackageName,
      _packageVersion = defaultVersion,
      _packageDependencies = [defaultStdlibDep buildDir],
      _packageMain = Nothing,
      _packageBuildDir = Nothing,
      _packageFile = yamlPath
    }

rawPackage :: Package -> RawPackage
rawPackage pkg =
  Package
    { _packageName = Just (pkg ^. packageName),
      _packageVersion = Just (prettySemVer (pkg ^. packageVersion)),
      _packageDependencies = Just (pkg ^. packageDependencies),
      _packageBuildDir = pkg ^. packageBuildDir,
      _packageMain = pkg ^. packageMain,
      _packageFile = ()
    }

processPackage :: forall r. (Members '[Error Text] r) => Path Abs File -> BuildDir -> RawPackage -> Sem r Package
processPackage _packageFile buildDir pkg = do
  let _packageName = fromMaybe defaultPackageName (pkg ^. packageName)
      base :: SomeBase Dir = (resolveBuildDir buildDir) <///> relStdlibDir
      stdlib = mkPathDependency (fromSomeDir base)
      _packageDependencies = fromMaybe [stdlib] (pkg ^. packageDependencies)
  checkNoDuplicateDepNames _packageDependencies
  _packageVersion <- getVersion
  return
    Package
      { _packageBuildDir = pkg ^. packageBuildDir,
        _packageMain = pkg ^. packageMain,
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
    checkNoDuplicateDepNames deps = go Set.empty (deps ^.. traversed . _GitDependency . gitDependencyName)
      where
        go :: Set Text -> [Text] -> Sem r ()
        go _ [] = return ()
        go s (x : xs)
          | x `Set.member` s = throw (errMsg x)
          | otherwise = go (Set.insert x s) xs
          where
            errMsg :: Text -> Text
            errMsg dupName =
              "Juvix package file at: "
                <> pack (toFilePath _packageFile)
                <> " contains the duplicate dependency name: "
                <> dupName

defaultStdlibDep :: BuildDir -> Dependency
defaultStdlibDep buildDir = mkPathDependency (fromSomeDir (resolveBuildDir buildDir <///> relStdlibDir))

defaultPackageName :: Text
defaultPackageName = "my-project"

defaultVersion :: SemVer
defaultVersion = SemVer 0 0 0 Nothing Nothing

globalPackage :: RawPackage
globalPackage =
  Package
    { _packageDependencies = Just [defaultStdlibDep DefaultBuildDir],
      _packageName = Just "global-juvix-package",
      _packageVersion = Just (prettySemVer defaultVersion),
      _packageMain = Nothing,
      _packageBuildDir = Nothing,
      _packageFile = ()
    }

mkPackageFilePath :: Path Abs Dir -> Path Abs File
mkPackageFilePath = (<//> juvixYamlFile)

-- | Given some directory d it tries to read the file d/juvix.yaml and parse its contents
readPackage ::
  forall r.
  (Members '[Files, Error Text] r) =>
  Path Abs Dir ->
  BuildDir ->
  Sem r Package
readPackage root buildDir = do
  bs <- readFileBS' yamlPath
  if
      | ByteString.null bs -> return (emptyPackage buildDir yamlPath)
      | otherwise -> either (throw . pack . prettyPrintParseException) (processPackage yamlPath buildDir) (decodeEither' bs)
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
