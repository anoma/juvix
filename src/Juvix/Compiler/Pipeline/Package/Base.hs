module Juvix.Compiler.Pipeline.Package.Base
  ( module Juvix.Compiler.Pipeline.Package.Base,
    module Juvix.Compiler.Pipeline.Package.Dependency,
  )
where

import Data.Aeson
import Data.Aeson.BetterErrors
import Data.Kind qualified as GHC
import Data.Versions hiding (Lens')
import Juvix.Compiler.Pipeline.Lockfile
import Juvix.Compiler.Pipeline.Package.Dependency
import Juvix.Extra.Paths
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude

data BuildDir
  = DefaultBuildDir
  | CustomBuildDir (SomeBase Dir)
  deriving stock (Eq, Show)

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
  PackageFileType 'Raw = Maybe ()
  PackageFileType 'Processed = Path Abs File

type PackageLockfileType :: IsProcessed -> GHC.Type
type family PackageLockfileType s = res | res -> s where
  PackageLockfileType 'Raw = Maybe ()
  PackageLockfileType 'Processed = Maybe LockfileInfo

data PackageId = PackageId
  { _packageIdName :: Text,
    _packageIdVersion :: SemVer
  }
  deriving stock (Show, Eq)

data Package' (s :: IsProcessed) = Package
  { _packageName :: NameType s,
    _packageVersion :: VersionType s,
    _packageDependencies :: DependenciesType s,
    _packageBuildDir :: Maybe (SomeBase Dir),
    _packageMain :: Maybe (Prepath File),
    _packageFile :: PackageFileType s,
    _packageLockfile :: PackageLockfileType s
  }
  deriving stock (Generic)

makeLenses ''Package'
makeLenses ''PackageId

type Package = Package' 'Processed

type RawPackage = Package' 'Raw

deriving stock instance Eq RawPackage

deriving stock instance Eq Package

deriving stock instance Show RawPackage

deriving stock instance Show Package

packageId :: Lens' Package PackageId
packageId (g :: PackageId -> f PackageId) pkg =
  let pkgId =
        PackageId
          { _packageIdName = pkg ^. packageName,
            _packageIdVersion = pkg ^. packageVersion
          }
   in toPackage <$> g pkgId
  where
    toPackage :: PackageId -> Package
    toPackage pkgid =
      pkg
        { _packageName = pkgid ^. packageIdName,
          _packageVersion = pkgid ^. packageIdVersion
        }

rawPackageOptions :: Options
rawPackageOptions =
  defaultOptions
    { fieldLabelModifier = over _head toLower . dropPrefix "_package",
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
        return Package {_packageFile = Nothing, _packageLockfile = Nothing, ..}
      err :: a
      err = error "Failed to parse juvix.yaml"

resolveBuildDir :: BuildDir -> SomeBase Dir
resolveBuildDir = \case
  DefaultBuildDir -> Rel relBuildDir
  CustomBuildDir d -> d

resolveAbsBuildDir :: Path Abs Dir -> BuildDir -> Path Abs Dir
resolveAbsBuildDir root = someBaseToAbs root . resolveBuildDir

mapCustomBuildDir :: (SomeBase Dir -> SomeBase Dir) -> BuildDir -> BuildDir
mapCustomBuildDir f = \case
  DefaultBuildDir -> DefaultBuildDir
  CustomBuildDir d -> CustomBuildDir (f d)

rootedBuildDir :: Path Abs Dir -> BuildDir -> BuildDir
rootedBuildDir root = mapCustomBuildDir (Abs . someBaseToAbs root)

-- | This is used when juvix.yaml exists but it is empty
emptyPackage :: BuildDir -> Path Abs File -> Package
emptyPackage buildDir yamlPath =
  Package
    { _packageName = defaultPackageName,
      _packageVersion = defaultVersion,
      _packageDependencies = [defaultStdlibDep buildDir],
      _packageMain = Nothing,
      _packageBuildDir = Nothing,
      _packageFile = yamlPath,
      _packageLockfile = Nothing
    }

rawPackage :: Package -> RawPackage
rawPackage pkg =
  Package
    { _packageName = Just (pkg ^. packageName),
      _packageVersion = Just (prettySemVer (pkg ^. packageVersion)),
      _packageDependencies = Just (pkg ^. packageDependencies),
      _packageBuildDir = pkg ^. packageBuildDir,
      _packageMain = pkg ^. packageMain,
      _packageFile = Nothing,
      _packageLockfile = Nothing
    }

defaultVersion :: SemVer
defaultVersion = SemVer 0 0 0 Nothing Nothing

unsetPackageLockfile :: Package -> Package
unsetPackageLockfile = set packageLockfile Nothing

defaultStdlibDep :: BuildDir -> Dependency
defaultStdlibDep buildDir = mkPathDependency (fromSomeDir (resolveBuildDir buildDir <///> relStdlibDir))

defaultPackageName :: Text
defaultPackageName = "my-project"

globalPackage :: Path Abs File -> Package
globalPackage p =
  Package
    { _packageDependencies = [defaultStdlibDep DefaultBuildDir],
      _packageName = "global-juvix-package",
      _packageVersion = defaultVersion,
      _packageMain = Nothing,
      _packageBuildDir = Nothing,
      _packageFile = p,
      _packageLockfile = Nothing
    }

packageBaseId :: PackageId
packageBaseId =
  PackageId
    { _packageIdName = Str.packageBase,
      _packageIdVersion = defaultVersion
    }

mkPackageFilePath :: Path Abs Dir -> Path Abs File
mkPackageFilePath = (<//> juvixYamlFile)

mkPackagePath :: Path Abs Dir -> Path Abs File
mkPackagePath = (<//> packageFilePath)
