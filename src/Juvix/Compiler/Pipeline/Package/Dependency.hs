module Juvix.Compiler.Pipeline.Package.Dependency
  ( Dependency (..),
    dependencyPath,
  )
where

import Data.Yaml
import Juvix.Prelude
import Juvix.Prelude.Pretty

newtype Dependency = Dependency
  { _dependencyPath :: Prepath Dir
  }
  deriving stock (Generic, Eq, Show)

instance ToJSON Dependency where
  toJSON (Dependency p) = toJSON p
  toEncoding (Dependency p) = toEncoding p

instance FromJSON Dependency where
  parseJSON = fmap Dependency . parseJSON

makeLenses ''Dependency

instance Pretty Dependency where
  pretty (Dependency i) = pretty i
