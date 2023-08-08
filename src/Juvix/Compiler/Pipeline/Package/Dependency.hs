module Juvix.Compiler.Pipeline.Package.Dependency where

import Data.Aeson (genericToEncoding, genericToJSON)
import Data.Aeson.BetterErrors
import Data.Aeson.BetterErrors qualified as Aeson
import Data.Aeson.TH
import Data.Text.Encoding.Error (lenientDecode)
import Data.Yaml
import Juvix.Prelude
import Juvix.Prelude.Pretty
import Lens.Micro.Platform qualified as Lens

data Dependency
  = DependencyPath PathDependency
  | DependencyGit GitDependency
  deriving stock (Generic, Eq, Show)

newtype PathDependency = PathDependency
  { _pathDependencyPath :: Prepath Dir
  }
  deriving stock (Generic, Eq, Show)

mkPathDependency :: FilePath -> Dependency
mkPathDependency = DependencyPath . PathDependency . mkPrepath

data GitDependency = GitDependency
  { _gitDependencyUrl :: Text,
    _gitDependencyRef :: Text,
    _gitDependencyName :: Text
  }
  deriving stock (Generic, Eq, Show)

makeLenses ''Dependency
makeLenses ''GitDependency
makeLenses ''PathDependency

instance Pretty PathDependency where
  pretty (PathDependency p) = pretty p

instance Pretty GitDependency where
  pretty g = pretty (decodeUtf8With lenientDecode (encode g))

instance Pretty Dependency where
  pretty = \case
    DependencyPath i -> pretty i
    DependencyGit g -> pretty g

instance ToJSON Dependency where
  toJSON = \case
    DependencyPath p -> toJSON p
    DependencyGit g -> toJSON g
  toEncoding = \case
    DependencyPath p -> toEncoding p
    DependencyGit g -> toEncoding g

instance FromJSON Dependency where
  parseJSON = toAesonParser' p
    where
      p :: Parse' Dependency
      p = (DependencyPath <$> fromAesonParser) Aeson.<|> (DependencyGit <$> fromAesonParser)

instance ToJSON PathDependency where
  toJSON (PathDependency p) = toJSON p
  toEncoding (PathDependency p) = toEncoding p

instance FromJSON PathDependency where
  parseJSON = fmap PathDependency . parseJSON

gitDependencyOptions :: Options
gitDependencyOptions =
  defaultOptions
    { fieldLabelModifier = over Lens._head toLower . dropPrefix "_gitDependency",
      rejectUnknownFields = True,
      omitNothingFields = True
    }

instance ToJSON GitDependency where
  toJSON = genericToJSON gitDependencyOptions
  toEncoding = genericToEncoding gitDependencyOptions

instance FromJSON GitDependency where
  parseJSON = toAesonParser' (key "git" p)
    where
      p :: Parse' GitDependency
      p = do
        _gitDependencyUrl <- key "url" asText
        _gitDependencyRef <- key "ref" asText
        _gitDependencyName <- key "name" asText
        return GitDependency {..}
