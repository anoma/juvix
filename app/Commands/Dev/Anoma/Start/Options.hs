module Commands.Dev.Anoma.Start.Options where

import Anoma.Client.Base
import CommonOptions

data StartOptions = StartOptions
  { _startAnomaPath :: AppPath Dir,
    _startLaunchMode :: LaunchMode
  }
  deriving stock (Data)

makeLenses ''StartOptions

parseStartOptions :: Parser StartOptions
parseStartOptions = do
  _startAnomaPath <- anomaDirOpt
  _startLaunchMode <-
    flag
      LaunchModeAttached
      LaunchModeDetached
      ( long "background"
          <> short 'b'
          <> help "Start the client in the background"
      )
  pure StartOptions {..}
