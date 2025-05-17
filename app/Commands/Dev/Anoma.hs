module Commands.Dev.Anoma
  ( module Commands.Dev.Anoma,
    module Commands.Dev.Anoma.Options,
  )
where

import Anoma.Client.Base
import Anoma.Client.Config
import Anoma.Effect.Base
import Commands.Base
import Commands.Dev.Anoma.AddTransaction.Options
import Commands.Dev.Anoma.Base
import Commands.Dev.Anoma.Client
import Commands.Dev.Anoma.Indexer qualified as Indexer
import Commands.Dev.Anoma.Options
import Commands.Dev.Anoma.PrintConfig qualified as PrintConfig
import Commands.Dev.Anoma.Prove qualified as Prove
import Commands.Dev.Anoma.Start qualified as Start
import Juvix.Data.CodeAnn
import Juvix.Data.Yaml qualified as Y

runCommand :: forall r. (Members AppEffects r) => AnomaCommandGlobal -> Sem r ()
runCommand g =
  runAppError @SimpleError $ case (g ^. anomaCommandGlobalCommand) of
    AnomaCommandStart opts -> Start.runCommand opts
    AnomaCommandStatus -> getClientConfig >>= renderStdOutLn . ppCodeAnn
    AnomaCommandStop -> getClientConfig >>= stopClient >> removeConfig
    AnomaCommandPrintConfig opts -> PrintConfig.runCommand opts
    AnomaCommandProve opts ->
      runAnomaWithHostConfig
        (Prove.runCommand opts)
    AnomaCommandAddTransaction opts ->
      runAnomaWithHostConfig
        (addTransaction (opts ^. addTransactionShielded) (opts ^. addTransactionFile))
    AnomaCommandIndexer opts -> runAnomaWithHostConfig (Indexer.runCommand opts)
  where
    runAnomaWithHostConfig :: (Members (Error SimpleError ': AppEffects) x) => Sem (Anoma ': x) () -> Sem x ()
    runAnomaWithHostConfig eff = do
      host <- getHostConfig
      runAnomaWithClient host eff

    getHostConfig :: (Members (Error SimpleError ': AppEffects) x) => Sem x AnomaClientInfo
    getHostConfig = case g ^. anomaCommandGlobalClientConfig of
      Just p -> fromAppFile p >>= readClientInfo
      Nothing -> (^. clientConfigHost) <$> getClientConfig

    readClientInfo :: (HasCallStack, Members '[Files, Error SimpleError] x) => Path Abs File -> Sem x AnomaClientInfo
    readClientInfo fp = do
      let pathName = pack (toFilePath fp)
      unlessM (fileExists' fp) (throw (SimpleError (mkAnsiText ("Config file: " <> pathName <> " does not exist"))))
      bs <- readFileBS' fp
      case Y.decodeEither bs of
        Left err -> throw (SimpleError (mkAnsiText (ghcCallStack <> "\nAeson error while parsing " <> toFilePath fp <> "\n" <> pack (Y.prettyPrintParseException err))))
        Right a -> return a
