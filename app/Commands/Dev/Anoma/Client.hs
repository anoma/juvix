module Commands.Dev.Anoma.Client where

import Anoma.Client.Config
import Commands.Base
import Data.Foldable.Extra qualified as E
import Juvix.Prelude.Posix

isClientRunning :: (Members '[Files, EmbedIO] r) => ClientConfig -> Sem r Bool
isClientRunning = isProcessRunning . (^. clientConfigPid)

checkClientRunning :: (Members '[Files, EmbedIO, Error SimpleError] r) => Sem r (Maybe ClientConfig)
checkClientRunning = do
  mconfig <- readConfig
  E.findM isClientRunning mconfig

stopClient :: (Members '[Files, EmbedIO] r) => ClientConfig -> Sem r ()
stopClient = terminateProcessPid . (^. clientConfigPid)
