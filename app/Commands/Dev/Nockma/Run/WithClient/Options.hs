module Commands.Dev.Nockma.Run.WithClient.Options where

import Commands.Dev.Anoma.Prove.Options.ProveArg
import CommonOptions

data NockmaRunWithClientOptions = NockmaRunWithClientOptions
  { _nockmaRunWithClientFile :: AppPath File,
    _nockmaRunWithClientPort :: Int,
    _nockmaRunWithClientNodeId :: Text,
    _nockmaRunWithClientUrl :: String,
    _nockmaRunWithClientArgs :: [ProveArg]
  }
  deriving stock (Data)

makeLenses ''NockmaRunWithClientOptions

parseNockmaRunWithClientOptions :: Parser NockmaRunWithClientOptions
parseNockmaRunWithClientOptions = do
  _nockmaRunWithClientFile <- parseInputFile FileExtNockma
  _nockmaRunWithClientArgs <- many parseProveArg
  _nockmaRunWithClientPort <-
    option
      (fromIntegral <$> naturalNumberOpt)
      ( long "http-port"
          <> short 'p'
          <> help ("The HTTP port of a running Anoma client")
          <> metavar "PORT"
      )
  _nockmaRunWithClientNodeId <-
    strOption
      ( long "node-id"
          <> short 'i'
          <> help ("The node id associated with the running Anoma client")
          <> metavar "NODE_ID"
      )
  _nockmaRunWithClientUrl <- do
    let defaultUrl :: String = "localhost"
    strOption
      ( long "url"
          <> help ("The URL of a running Anoma client. default: " <> defaultUrl)
          <> value defaultUrl
          <> metavar "URL"
      )
  pure NockmaRunWithClientOptions {..}
