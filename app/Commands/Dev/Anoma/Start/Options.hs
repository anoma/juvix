module Commands.Dev.Anoma.Start.Options where

import Anoma.Client.Base
import CommonOptions

data StartOptions = StartOptions
  { _startAnomaPath :: AppPath Dir,
    _startLaunchMode :: LaunchMode,
    _startForce :: Bool
  }
  deriving stock (Data)

makeLenses ''StartOptions

parseStartOptions :: Parser StartOptions
parseStartOptions = do
  _startAnomaPath <- anomaDirOpt
  _startLaunchMode <-
    flag
      LaunchModeDetached
      LaunchModeAttached
      ( long "foreground"
          <> short 'g'
          <> help "Start the client in the foreground"
      )
  _startForce <-
    switch
      ( long "force"
          <> short 'f'
          <> help "Forcefully start a client, terminating any currently running client if necessary"
      )
  pure StartOptions {..}
