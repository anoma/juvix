module Juvix.Compiler.Pipeline.Package
  ( module Juvix.Compiler.Pipeline.Package,
    module Juvix.Compiler.Pipeline.Package.Dependency,
  )
where

import Data.Aeson.BetterErrors
import Data.Aeson.TH
import Data.Yaml
import Juvix.Compiler.Pipeline.Package.Dependency
import Juvix.Extra.Paths
import Juvix.Prelude
import Juvix.Prelude.Path
import Lens.Micro.Platform qualified as Lens

data Package = Package
  { _packageName :: Maybe Text,
    _packageVersion :: Maybe Text,
    _packageDependencies :: [Dependency]
  }
  deriving stock (Eq, Show, Generic)

$( deriveToJSON
     defaultOptions
       { fieldLabelModifier = over Lens._head toLower . dropPrefix "_package",
         rejectUnknownFields = True
       }
     ''Package
 )

instance FromJSON Package where
  parseJSON = toAesonParser' (fromMaybe emptyPackage <$> p)
    where
      p :: Parse' (Maybe Package)
      p = perhaps $ do
        _packageName <- keyMay "name" asText
        _packageVersion <- keyMay "version" asText
        _packageDependencies <- fromMaybe [] <$> keyMay "dependencies" fromAesonParser
        return Package {..}

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
packageVersion' f p = (\(Const r) -> Const r) (f ver)
  where
    ver :: Text
    ver = fromMaybe "no version" (p ^. packageVersion)

-- | given some directory d it tries to read the file d/juvix.yaml and parse its contents
readPackage :: forall r. Members '[Files, Error Text] r => Path Abs Dir -> Sem r Package
readPackage adir = do
  bs <- readFileBS' yamlPath
  either (throw . pack . prettyPrintParseException) (mkAbsPaths dir) (decodeEither' bs)
  where
    dir = toFilePath adir
    yamlPath = dir </> juvixYamlFile
    mkAbsPaths :: FilePath -> Package -> Sem r Package
    mkAbsPaths root pkg = traverseOf (packageDependencies . each . dependencyPath) go pkg
      where
        go :: FilePath -> Sem r FilePath
        go p = canonicalizePath' (root </> p)

readPackageIO :: Path Abs Dir -> IO Package
readPackageIO dir = do
  d <- getCurrentDirectory
  let x :: Sem '[Error Text, Files, Embed IO] Package
      x = readPackage dir
  m <- runM $ runError $ runFilesIO d (runError x)
  case m of
    Left err -> runM (runReader defaultGenericOptions (printErrorAnsiSafe err)) >> exitFailure
    Right (Left err) -> putStrLn err >> exitFailure
    Right (Right r) -> return r
