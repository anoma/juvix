module Juvix.Compiler.Pipeline.Lockfile where

import Data.Aeson.BetterErrors
import Data.Aeson.BetterErrors qualified as Aeson
import Data.Aeson.Encoding (pair)
import Data.Aeson.TH
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Yaml
import Data.Yaml.Pretty
import Juvix.Compiler.Pipeline.Package.Dependency
import Juvix.Compiler.Pipeline.Package.Loader.Error
import Juvix.Extra.Paths
import Juvix.Extra.Strings qualified as Str
import Juvix.Extra.Version
import Juvix.Prelude hiding ((.=))
import Juvix.Prelude.Aeson
import Lens.Micro.Platform qualified as Lens

data LockfileDependency = LockfileDependency
  { _lockfileDependencyDependency :: Dependency,
    _lockfileDependencyDependencies :: [LockfileDependency]
  }
  deriving stock (Generic, Show, Eq)

newtype Lockfile = Lockfile
  { _lockfileDependencies :: [LockfileDependency]
  }
  deriving stock (Generic, Show, Eq)

type LockfileV1 = Lockfile

data LockfileV2 = LockfileV2
  { _lockfileV2Version :: Int,
    _lockfileV2Checksum :: Text,
    _lockfileV2Dependencies :: [LockfileDependency]
  }
  deriving stock (Generic, Show, Eq)

data VersionedLockfile
  = V1Lockfile LockfileV1
  | V2Lockfile LockfileV2
  deriving stock (Show, Eq)

data LockfileVersionTag
  = LockfileV1Tag
  | LockfileV2Tag
  deriving stock (Show, Eq)

allVersionedLockfiles :: [LockfileVersionTag]
allVersionedLockfiles = [LockfileV1Tag, LockfileV2Tag]

lockfileVersionNumber :: LockfileVersionTag -> Int
lockfileVersionNumber = \case
  LockfileV1Tag -> 1
  LockfileV2Tag -> 2

data LockfileInfo = LockfileInfo
  { _lockfileInfoPath :: Path Abs File,
    _lockfileInfoChecksum :: Maybe Text,
    _lockfileInfoLockfile :: Lockfile
  }
  deriving stock (Eq, Show)

makeLenses ''LockfileDependency
makeLenses ''Lockfile
makeLenses ''LockfileV2
makeLenses ''LockfileInfo

mkLockfileV2 :: Text -> [LockfileDependency] -> LockfileV2
mkLockfileV2 _lockfileV2Checksum _lockfileV2Dependencies =
  LockfileV2 {_lockfileV2Version = lockfileVersionNumber LockfileV2Tag, ..}

lockfile' :: SimpleGetter VersionedLockfile Lockfile
lockfile' = to $ \case
  V1Lockfile l -> l
  V2Lockfile l -> Lockfile {_lockfileDependencies = l ^. lockfileV2Dependencies}

checksum :: SimpleGetter VersionedLockfile (Maybe Text)
checksum = to $ \case
  V1Lockfile {} -> Nothing
  V2Lockfile l -> Just (l ^. lockfileV2Checksum)

instance ToJSON LockfileDependency where
  toJSON d = object [dep, Str.dependencies .= toJSON (d ^. lockfileDependencyDependencies)]
    where
      dep :: (Key, Value)
      dep = case d ^. lockfileDependencyDependency of
        DependencyGit g -> (Str.git, toJSON g)
        DependencyPath p -> (Str.path_, toJSON p)
  toEncoding d = pairs (dep <> pair Str.dependencies (toEncoding (d ^. lockfileDependencyDependencies)))
    where
      dep :: Series
      dep = case d ^. lockfileDependencyDependency of
        DependencyGit g -> pair Str.git (toEncoding g)
        DependencyPath p -> pair Str.path_ (toEncoding p)

instance FromJSON LockfileDependency where
  parseJSON = toAesonParser' p
    where
      p :: Parse' LockfileDependency
      p = do
        _lockfileDependencyDependency <- p'
        _lockfileDependencyDependencies <- key Str.dependencies fromAesonParser
        return LockfileDependency {..}

      p' :: Parse' Dependency
      p' = DependencyPath <$> (key Str.path_ fromAesonParser) Aeson.<|> DependencyGit <$> (key Str.git fromAesonParser)

lockfileV2Options :: Options
lockfileV2Options =
  defaultOptions
    { fieldLabelModifier = over Lens._head toLower . dropPrefix "_lockfileV2",
      rejectUnknownFields = True,
      omitNothingFields = True
    }

instance ToJSON LockfileV2 where
  toJSON = genericToJSON lockfileV2Options
  toEncoding = genericToEncoding lockfileV2Options

data LockfileParseErr = LockfileUnsupportedVersion

instance FromJSON VersionedLockfile where
  parseJSON = toAesonParser displayErr $ do
    v <- parseVersion
    case v of
      LockfileV1Tag -> V1Lockfile <$> parseV1
      LockfileV2Tag -> V2Lockfile <$> parseV2
    where
      parseV1 :: Parse LockfileParseErr Lockfile
      parseV1 = Lockfile <$> key Str.dependencies fromAesonParser

      parseV2 :: Parse LockfileParseErr LockfileV2
      parseV2 = do
        checksum' <- key "checksum" asText
        deps <- key Str.dependencies fromAesonParser
        return (mkLockfileV2 checksum' deps)

      parseVersion :: Parse LockfileParseErr LockfileVersionTag
      parseVersion = fromMaybeM (return LockfileV1Tag) (keyMay Str.version parseVersion')
        where
          parseVersion' :: Parse LockfileParseErr LockfileVersionTag
          parseVersion' = do
            n <- asIntegral
            if
                | lockfileVersionNumber LockfileV1Tag == n -> return LockfileV1Tag
                | lockfileVersionNumber LockfileV2Tag == n -> return LockfileV2Tag
                | otherwise -> throwCustomError LockfileUnsupportedVersion

      displayErr :: LockfileParseErr -> Text
      displayErr = \case
        LockfileUnsupportedVersion -> "lockfile error: unsupported version. Supported versions: " <> T.intercalate ", " (show . lockfileVersionNumber <$> allVersionedLockfiles)

mkPackageLockfilePath :: Path Abs Dir -> Path Abs File
mkPackageLockfilePath = (<//> juvixLockfile)

mayReadLockfile ::
  forall r.
  (Members '[Files, Error PackageLoaderError] r) =>
  Path Abs Dir ->
  Sem r (Maybe LockfileInfo)
mayReadLockfile root = do
  lockfileExists <- fileExists' lockfilePath
  if
      | lockfileExists -> do
          bs <- readFileBS' lockfilePath
          either (throwErr . pack . prettyPrintParseException) ((return . Just) . mkLockfileInfo lockfilePath) (decodeEither' @VersionedLockfile bs)
      | otherwise -> return Nothing
  where
    mkLockfileInfo :: Path Abs File -> VersionedLockfile -> LockfileInfo
    mkLockfileInfo _lockfileInfoPath vl =
      LockfileInfo
        { _lockfileInfoChecksum = vl ^. checksum,
          _lockfileInfoLockfile = vl ^. lockfile',
          ..
        }

    lockfilePath :: Path Abs File
    lockfilePath = mkPackageLockfilePath root

    throwErr :: Text -> Sem r a
    throwErr e =
      throw
        PackageLoaderError
          { _packageLoaderErrorPath = lockfilePath,
            _packageLoaderErrorCause =
              ErrLockfileYamlParseError
                LockfileYamlParseError {_lockfileYamlParseErrorError = e}
          }

lockfileEncodeConfig :: Config
lockfileEncodeConfig = setConfCompare keyCompare defConfig
  where
    -- serialize the dependencies field after all other keys and the version
    -- field before all other keys
    keyCompare :: Text -> Text -> Ordering
    keyCompare x y =
      if
          | y == Str.dependencies -> LT
          | x == Str.dependencies -> GT
          | x == Str.version -> LT
          | y == Str.version -> GT
          | otherwise -> compare x y

writeLockfile :: (Members '[Files] r) => Path Abs File -> Text -> Lockfile -> Sem r ()
writeLockfile lockfilePath checksum' lf = do
  ensureDir' (parent lockfilePath)
  let v2lf = mkLockfileV2 checksum' (lf ^. lockfileDependencies)
  writeFileBS lockfilePath (header <> encodePretty lockfileEncodeConfig v2lf)
  where
    header :: ByteString
    header = [i|\# This file was autogenerated by Juvix version #{versionDoc}.\n\# Do not edit this file manually.\n\n|]

-- | Extract a lockfileInfo associated with an immediate dependency. Returns Nothing
-- if the dependency is not specified at the root of the lockfile.
extractLockfileInfo :: LockfileInfo -> Dependency -> Maybe LockfileInfo
extractLockfileInfo lf d = mkLockfileInfo . (^. lockfileDependencyDependencies) <$> foundDep
  where
    foundDep :: Maybe LockfileDependency
    foundDep = find go (lf ^. lockfileInfoLockfile . lockfileDependencies)

    go :: LockfileDependency -> Bool
    go ld = case (d, ld ^. lockfileDependencyDependency) of
      (DependencyGit dg, DependencyGit ldg) -> dg ^. gitDependencyUrl == ldg ^. gitDependencyUrl
      (DependencyPath dp, DependencyPath ldp) -> dp ^. pathDependencyPath == ldp ^. pathDependencyPath
      _ -> False

    mkLockfileInfo :: [LockfileDependency] -> LockfileInfo
    mkLockfileInfo _lockfileDependencies = lf {_lockfileInfoLockfile = Lockfile {..}}
