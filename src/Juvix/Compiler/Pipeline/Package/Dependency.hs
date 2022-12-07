module Juvix.Compiler.Pipeline.Package.Dependency
  ( RawDependency,
    Dependency,
    Dependency' (..),
    processDependency,
    stdlibDefaultDep,
    dependencyPath,
    rawDependency,
  )
where

-- import Lens.Micro.Platform qualified as Lens
import Data.Aeson.BetterErrors
import Data.Kind qualified as GHC
import Data.Yaml
import Juvix.Extra.Paths
import Juvix.Prelude

type PathType :: IsProcessed -> GHC.Type
type family PathType s = res | res -> s where
  PathType 'Raw = SomeBase Dir
  PathType 'Processed = Path Abs Dir

-- | dependencies paths are canonicalized just after reading the package
newtype Dependency' (s :: IsProcessed) = Dependency
  { _dependencyPath :: PathType s
  }
  deriving stock (Generic)

type RawDependency = Dependency' 'Raw

type Dependency = Dependency' 'Processed

deriving stock instance Eq RawDependency

deriving stock instance Eq Dependency

deriving stock instance Show RawDependency

deriving stock instance Show Dependency

instance ToJSON RawDependency where
  toEncoding (Dependency p) = toEncoding (fromSomeDir p)

instance FromJSON RawDependency where
  parseJSON = toAesonParser id (Dependency <$> p)
    where
      p :: Parse Text (SomeBase Dir)
      p = do
        str <- asString
        let dir = parseSomeDir str
        maybe (throwCustomError ("failed to parse directory: " <> pack str)) pure dir

rawDependency :: Dependency -> RawDependency
rawDependency (Dependency p) = Dependency (Abs p)

processDependency :: Path Abs Dir -> RawDependency -> Dependency
processDependency r (Dependency p) = case p of
  Rel a -> Dependency (r <//> a)
  Abs a -> Dependency a

stdlibDefaultDep :: RawDependency
stdlibDefaultDep = Dependency (Rel juvixStdlibDir')

makeLenses ''Dependency'
