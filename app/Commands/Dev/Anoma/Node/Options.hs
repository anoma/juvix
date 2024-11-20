module Commands.Dev.Anoma.Node.Options where

import Anoma.Client.Base
import CommonOptions

data NodeOptions = NodeOptions
  { _nodeAnomaPath :: AppPath Dir,
    _nodeLaunchMode :: LaunchMode
  }
  deriving stock (Data)

makeLenses ''NodeOptions

parseNodeOptions :: Parser NodeOptions
parseNodeOptions = do
  _nodeAnomaPath <- anomaDirOpt
  _nodeLaunchMode <-
    flag
      LaunchModeAttached
      LaunchModeDetached
      ( long "background"
          <> short 'b'
          <> help "Launch the client in the background"
      )
  pure NodeOptions {..}
