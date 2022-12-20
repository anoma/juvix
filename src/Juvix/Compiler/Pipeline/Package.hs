module Juvix.Compiler.Pipeline.Package
  ( module Juvix.Compiler.Pipeline.Package.Dependency,
    RawPackage,
    Package,
    Package' (..),
    defaultPackage,
    packageName,
    packageVersion,
    packageDependencies,
    rawPackage,
    emptyPackage,
    readPackage,
    readPackageIO,
  )
where

import Data.Aeson (genericToEncoding)
import Data.Aeson.BetterErrors
import Data.Aeson.TH
import Data.Kind qualified as GHC
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
  VersionType 'Processed = Versioning

type DependenciesType :: IsProcessed -> GHC.Type
type family DependenciesType s = res | res -> s where
  DependenciesType 'Raw = Maybe [RawDependency]
  DependenciesType 'Processed = [Dependency]

data Package' (s :: IsProcessed) = Package
  { _packageName :: NameType s,
    _packageVersion :: VersionType s,
    _packageDependencies :: DependenciesType s
  }
  deriving stock (Generic)

makeLenses ''Package'

type Package = Package' 'Processed

type RawPackage = Package' 'Raw

deriving stock instance Eq RawPackage

deriving stock instance Eq Package

deriving stock instance Show RawPackage

deriving stock instance Show Package

instance ToJSON RawPackage where
  toEncoding = genericToEncoding options
    where
      options :: Options
      options =
        defaultOptions
          { fieldLabelModifier = over Lens._head toLower . dropPrefix "_package",
            rejectUnknownFields = True
          }

-- | TODO: is it a good idea to return the empty package if it fails to parse?
instance FromJSON RawPackage where
  parseJSON = toAesonParser' (fromMaybe rawDefaultPackage <$> p)
    where
      p :: Parse' (Maybe RawPackage)
      p = perhaps $ do
        _packageName <- keyMay "name" asText
        _packageVersion <- keyMay "version" asText
        _packageDependencies <- keyMay "dependencies" fromAesonParser
        return Package {..}

-- | Has the implicit stdlib dependency
rawDefaultPackage :: Package' 'Raw
rawDefaultPackage =
  Package
    { _packageName = Nothing,
      _packageVersion = Nothing,
      _packageDependencies = Nothing
    }

-- | Has the implicit stdlib dependency
defaultPackage :: Path Abs Dir -> Package
defaultPackage r = fromRight impossible . run . runError @Text . processPackage r $ rawDefaultPackage

-- | Has no dependencies
emptyPackage :: Package
emptyPackage =
  Package
    { _packageName = defaultPackageName,
      _packageVersion = Ideal defaultVersion,
      _packageDependencies = []
    }

rawPackage :: Package -> RawPackage
rawPackage pkg =
  Package
    { _packageName = Just (pkg ^. packageName),
      _packageVersion = Just (prettyV (pkg ^. packageVersion)),
      _packageDependencies = Just (map rawDependency (pkg ^. packageDependencies))
    }

processPackage :: forall r. Members '[Error Text] r => Path Abs Dir -> Package' 'Raw -> Sem r Package
processPackage dir pkg = do
  let _packageName = fromMaybe defaultPackageName (pkg ^. packageName)
      _packageDependencies =
        let rawDeps :: [RawDependency] = fromMaybe [stdlibDefaultDep] (pkg ^. packageDependencies)
         in map (processDependency dir) rawDeps
  _packageVersion <- getVersion
  return Package {..}
  where
    getVersion :: Sem r Versioning
    getVersion = case pkg ^. packageVersion of
      Nothing -> return (Ideal defaultVersion)
      Just ver -> case versioning ver of
        Right v -> return v
        Left err -> throw (pack (errorBundlePretty err))

defaultPackageName :: Text
defaultPackageName = "my-project"

defaultVersion :: SemVer
defaultVersion = SemVer 0 0 0 [] Nothing

-- | given some directory d it tries to read the file d/juvix.yaml and parse its contents
readPackage :: forall r. Members '[Files, Error Text] r => Path Abs Dir -> Sem r Package
readPackage adir = do
  bs <- readFileBS' yamlPath
  either (throw . pack . prettyPrintParseException) (processPackage adir) (decodeEither' bs)
  where
    yamlPath = adir <//> juvixYamlFile

readPackageIO :: Path Abs Dir -> IO Package
readPackageIO dir = do
  let x :: Sem '[Error Text, Files, Embed IO] Package
      x = readPackage dir
  m <- runM $ runError $ runFilesIO (runError x)
  case m of
    Left err -> runM (runReader defaultGenericOptions (printErrorAnsiSafe err)) >> exitFailure
    Right (Left err) -> putStrLn err >> exitFailure
    Right (Right r) -> return r
