module Anoma.Client.Config where

import Anoma.Client.Base
import Data.Text qualified as T
import Juvix.Data.CodeAnn
import Juvix.Data.Yaml qualified as Y
import Juvix.Extra.Paths.Base (clientConfigPath)
import Juvix.Prelude
import Juvix.Prelude.Aeson as Aeson

data ClientConfig = ClientConfig
  { _clientConfigPid :: Int,
    _clientConfigHost :: AnomaClientInfo
  }

makeLenses ''ClientConfig

$( deriveJSON
     Aeson.defaultOptions
       { unwrapUnaryRecords = True,
         allowOmittedFields = False,
         rejectUnknownFields = True,
         fieldLabelModifier = \case
           "_clientConfigPid" -> "pid"
           "_clientConfigHost" -> "host"
           _ -> impossibleError "All fields must be covered"
       }
     ''ClientConfig
 )

instance PrettyCodeAnn ClientConfig where
  ppCodeAnn c =
    -- The output of YAML encoding has a trailing newline
    pretty (T.dropWhileEnd (== '\n') (decodeUtf8 (Y.encode c)))

configPath :: (Member Files r) => Sem r (Path Abs File)
configPath = (<//> clientConfigPath) <$> globalAnomaClient

writeConfig :: (Member Files r) => ClientConfig -> Sem r ()
writeConfig conf = do
  cp <- configPath
  ensureDir' (parent cp)
  writeFileBS cp (Y.encode conf)

readConfig :: (Members '[Files, Error SimpleError] r) => Sem r (Maybe ClientConfig)
readConfig = do
  cp <- configPath
  whenMaybeM (fileExists' cp) $ do
    bs <- readFileBS' cp
    case Y.decodeEither bs of
      Left err -> throw (SimpleError (mkAnsiText (Y.prettyPrintParseException err <> "\n" <> toFilePath cp)))
      Right a -> return a

removeConfig :: (Members '[Files] r) => Sem r ()
removeConfig = do
  cp <- configPath
  whenM (fileExists' cp) (removeFile' cp)
