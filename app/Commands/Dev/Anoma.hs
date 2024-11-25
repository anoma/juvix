module Commands.Dev.Anoma
  ( module Commands.Dev.Anoma,
    module Commands.Dev.Anoma.Options,
  )
where

import Anoma.Client.Config
import Anoma.Effect.Base
import Commands.Base
import Commands.Dev.Anoma.Client
import Commands.Dev.Anoma.Options
import Commands.Dev.Anoma.Prove qualified as Prove
import Commands.Dev.Anoma.Prove.Options
import Commands.Dev.Anoma.Start qualified as Start
import Juvix.Data.CodeAnn
import Juvix.Data.Yaml qualified as Y

runCommand :: forall r. (Members AppEffects r) => AnomaCommand -> Sem r ()
runCommand =
  runAppError @SimpleError . \case
    AnomaCommandStart opts -> Start.runCommand opts
    AnomaCommandStatus -> checkRunning >>= renderStdOutLn . ppCodeAnn
    AnomaCommandStop -> checkRunning >>= stopClient >> removeConfig
    AnomaCommandProve opts -> do
      host <- getHostConfig (opts ^. proveClientInfo)
      runAnomaWithClient host (Prove.runCommand opts)
  where
    checkRunning :: (Members (Error SimpleError ': AppEffects) x) => Sem x ClientConfig
    checkRunning = fromMaybeM (logInfo "The Anoma client is not running" >> exitFailure) checkClientRunning

    getHostConfig :: (Members (Error SimpleError ': AppEffects) x) => Maybe (AppPath File) -> Sem x AnomaClientInfo
    getHostConfig = \case
      Just p -> fromAppFile p >>= readClientInfo
      Nothing -> (^. clientConfigHost) <$> checkRunning

    readClientInfo :: (Members '[Files, Error SimpleError] x) => Path Abs File -> Sem x AnomaClientInfo
    readClientInfo fp = do
      let pathName = pack (toFilePath fp)
      unlessM (fileExists' fp) (throw (SimpleError (mkAnsiText ("Config file: " <> pathName <> " does not exist"))))
      bs <- readFileBS' fp
      case Y.decodeEither bs of
        Left err -> throw (SimpleError (mkAnsiText (Y.prettyPrintParseException err <> "\n" <> toFilePath fp)))
        Right a -> return a
