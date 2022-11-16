module Juvix.Compiler.Pipeline.Package
  ( module Juvix.Compiler.Pipeline.Package,
    module Juvix.Compiler.Pipeline.Package.Dependency,
  )
where

import Juvix.Extra.Paths
import Data.Yaml
import Data.Aeson.TH
import Juvix.Compiler.Pipeline.Package.Dependency
import Juvix.Prelude
import Lens.Micro.Platform qualified as Lens

data Package = Package
  { _packageName :: Maybe Text,
    _packageVersion :: Maybe Text,
    _packageDependencies :: [Dependency]
  }
  deriving stock (Eq, Show, Generic)

$( deriveJSON
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
      _packageVersion = Nothing,
      _packageDependencies = mempty
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

-- | given some directory d it tries to read the file d/juvix.yaml and parse its contents
readPackage :: Members '[Files, Error Text] r => FilePath -> Sem r Package
readPackage dir = do
   bs <- readFileBS' yamlPath
   either (throw . pack . prettyPrintParseException) return (decodeEither' bs)
   where
    yamlPath = dir </> juvixYamlFile
