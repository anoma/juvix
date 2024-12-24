module Commands.Dev.Anoma.Client
  ( module Commands.Dev.Anoma.Client,
    module Anoma.Client.Config,
  )
where

import Anoma.Client.Config
import Anoma.Effect.Base
import Commands.Base
import Data.Foldable.Extra qualified as E
import Juvix.Prelude.Posix

isClientRunning :: (Members '[Files, EmbedIO, Error SimpleError, Logger] r) => ClientConfig -> Sem r Bool
isClientRunning c =
  runAnomaWithClient
    (c ^. clientConfigHost)
    (catchError @SimpleError (anomaListMethods >> return True) (\_ _ -> return False))

checkClientRunning :: (Members '[Logger, Files, EmbedIO, Error SimpleError] r) => Sem r (Maybe ClientConfig)
checkClientRunning = do
  mconfig <- readConfig
  E.findM isClientRunning mconfig

getClientConfig ::
  (Members '[Logger, Files, EmbedIO, Error SimpleError] r) =>
  Sem r ClientConfig
getClientConfig = fromMaybeM (throw @SimpleError "The Anoma client is not running") checkClientRunning

stopClient :: (Members '[Files, EmbedIO] r) => ClientConfig -> Sem r ()
stopClient = terminateProcessPid . (^. clientConfigPid)
