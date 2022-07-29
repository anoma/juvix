module Juvix.Pipeline.Package where

import Data.Aeson.TH
import Juvix.Prelude
import Lens.Micro.Platform qualified as Lens

data Package = Package
  { _packageName :: Maybe Text,
    _packageVersion :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

$( deriveFromJSON
     defaultOptions
       { fieldLabelModifier = over Lens._head toLower . dropPrefix "_package",
         rejectUnknownFields = True
       }
     ''Package
 )

emptyPackage :: Package
emptyPackage =
  Package
    { _packageName = Nothing,
      _packageVersion = Nothing
    }

makeLenses ''Package

packageName' :: Getting r Package Text
packageName' f p = (\(Const r) -> Const r) (f name)
  where
    name :: Text
    name = fromMaybe "my-package" (p ^. packageName)

packageVersion' :: Getting r Package Text
packageVersion' f p = (\(Const r) -> Const r) (f name)
  where
    name :: Text
    name = fromMaybe "no version" (p ^. packageVersion)
