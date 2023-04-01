module Juvix.Compiler.Pipeline.Package.Dependency
  ( Dependency (..),
    dependencyPath,
  )
where

import Data.Aeson.BetterErrors
import Data.Yaml
import Juvix.Prelude
import Juvix.Prelude.Pretty

newtype Dependency = Dependency
  { _dependencyPath :: SomeBase Dir
  }
  deriving stock (Generic, Eq, Show)

instance ToJSON Dependency where
  toJSON (Dependency p) = toJSON (fromSomeDir p)
  toEncoding (Dependency p) = toEncoding (fromSomeDir p)

instance FromJSON Dependency where
  parseJSON = toAesonParser id (Dependency <$> p)
    where
      p :: Parse Text (SomeBase Dir)
      p = do
        str <- asString
        let dir = parseSomeDir str
        maybe (throwCustomError ("failed to parse directory: " <> pack str)) pure dir

makeLenses ''Dependency

instance Pretty Dependency where
  pretty (Dependency i) = pretty (fromSomeDir i)
