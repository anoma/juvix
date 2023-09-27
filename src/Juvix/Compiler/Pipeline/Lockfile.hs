module Juvix.Compiler.Pipeline.Lockfile where

import Data.Aeson.BetterErrors
import Data.Aeson.BetterErrors qualified as Aeson
import Data.Aeson.Encoding (pair)
import Data.Aeson.TH
import Juvix.Compiler.Pipeline.Package.Dependency
import Juvix.Extra.Strings qualified as Str
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

makeLenses ''LockfileDependency
makeLenses ''Lockfile

instance ToJSON LockfileDependency where
  toJSON i = object [dep, Str.dependencies .= toJSON (i ^. lockfileDependencyDependencies)]
    where
      dep :: (Key, Value)
      dep = case i ^. lockfileDependencyDependency of
        DependencyGit g -> (Str.git, toJSON g)
        DependencyPath p -> (Str.path_, toJSON p)
  toEncoding i = pairs (dep <> pair Str.dependencies (toEncoding (i ^. lockfileDependencyDependencies)))
    where
      dep :: Series
      dep = case i ^. lockfileDependencyDependency of
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

lockfileOptions :: Options
lockfileOptions =
  defaultOptions
    { fieldLabelModifier = over Lens._head toLower . dropPrefix "_lockfile",
      rejectUnknownFields = True,
      omitNothingFields = True
    }

instance ToJSON Lockfile where
  toJSON = genericToJSON lockfileOptions
  toEncoding = genericToEncoding lockfileOptions

instance FromJSON Lockfile where
  parseJSON = toAesonParser' (Lockfile <$> fromAesonParser)

-- | Extract a lockfile associated with an immediate dependency. Returns Nothing
-- if the dependency is not specified at the root of the lockfile.
extractLockfile :: Lockfile -> Dependency -> Maybe Lockfile
extractLockfile lf d = Lockfile . (^. lockfileDependencyDependencies) <$> foundDep
  where
    foundDep :: Maybe LockfileDependency
    foundDep = find go (lf ^. lockfileDependencies)

    go :: LockfileDependency -> Bool
    go ld = case (d, ld ^. lockfileDependencyDependency) of
      (DependencyGit dg, DependencyGit ldg) -> dg ^. gitDependencyUrl == ldg ^. gitDependencyUrl
      (DependencyPath dp, DependencyPath ldp) -> dp ^. pathDependencyPath == ldp ^. pathDependencyPath
      _ -> False
