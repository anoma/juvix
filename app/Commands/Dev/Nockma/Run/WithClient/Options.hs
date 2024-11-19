module Commands.Dev.Nockma.Run.WithClient.Options where

import CommonOptions

data NockmaRunWithClientOptions = NockmaRunWithClientOptions
  { _nockmaRunWithClientFile :: AppPath File,
    _nockmaRunWithClientGrpcPort :: Int,
    _nockmaRunWithClientUrl :: String,
    _nockmaRunWithClientArgs :: Maybe (AppPath File)
  }
  deriving stock (Data)

makeLenses ''NockmaRunWithClientOptions

parseNockmaRunWithClientOptions :: Parser NockmaRunWithClientOptions
parseNockmaRunWithClientOptions = do
  _nockmaRunWithClientFile <- parseInputFile FileExtNockma
  _nockmaRunWithClientArgs <- optional anomaArgsOpt
  _nockmaRunWithClientGrpcPort <-
    option
      (fromIntegral <$> naturalNumberOpt)
      ( long "grpc-port"
          <> short 'p'
          <> help ("The GRPC port of a running Anoma client")
          <> metavar "PORT"
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
