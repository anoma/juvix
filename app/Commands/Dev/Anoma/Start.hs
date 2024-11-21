module Commands.Dev.Anoma.Start where

import Anoma.Client.Config
import Anoma.Effect
import Commands.Base
import Commands.Dev.Anoma.Client
import Commands.Dev.Anoma.Start.Options
import Juvix.Data.CodeAnn

runCommand :: forall r. (Members AppEffects r) => StartOptions -> Sem r ()
runCommand opts = runAppError @SimpleError
  . runProcess
  $ case opts ^. startLaunchMode of
    LaunchModeAttached -> go >>= void . waitForProcess >> removeConfig
    LaunchModeDetached -> void go
  where
    go :: forall x. (Members (Process ': Error SimpleError ': AppEffects) x) => Sem x ProcessHandle
    go = do
      whenJustM checkClientRunning $ \config ->
        if
            | (opts ^. startForce) -> stopClient config
            | otherwise ->
                throw
                  ( SimpleError
                      ( mkAnsiText
                          ( "An Anoma client is already running"
                              <> line
                              <> line
                              <> ppCodeAnn config
                          )
                      )
                  )
      i <- startClient
      let processH = i ^. anomaClientLaunchInfoProcess . anomaProcessHandle
      mpid <- getPid processH
      case mpid of
        Just pid -> updateConfig pid (i ^. anomaClientLaunchInfoInfo) >> return processH
        Nothing -> throw (SimpleError "The Anoma client did not start sucessfully")
      where
        startClient :: Sem x AnomaClientLaunchInfo
        startClient = do
          let launchMode = opts ^. startLaunchMode
          anomaDir :: AnomaPath <- AnomaPath <$> fromAppPathDir (opts ^. startAnomaPath)
          launchAnomaClient launchMode anomaDir

        updateConfig :: Pid -> AnomaClientInfo -> Sem x ()
        updateConfig pid clientInfo =
          writeConfig
            ClientConfig
              { _clientConfigHost = clientInfo,
                _clientConfigPid = fromIntegral pid
              }
