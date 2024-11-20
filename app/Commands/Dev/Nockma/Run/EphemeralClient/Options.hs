module Commands.Dev.Nockma.Run.EphemeralClient.Options where

import CommonOptions

data NockmaRunEphemeralClientOptions = NockmaRunEphemeralClientOptions
  { _nockmaRunEphemeralFile :: AppPath File,
    _nockmaRunEphemeralAnomaDir :: AppPath Dir,
    _nockmaRunEphemeralArgs :: Maybe (AppPath File)
  }
  deriving stock (Data)

makeLenses ''NockmaRunEphemeralClientOptions

parseNockmaRunEphemeralClientOptions :: Parser NockmaRunEphemeralClientOptions
parseNockmaRunEphemeralClientOptions = do
  _nockmaRunEphemeralFile <- parseInputFile FileExtNockma
  _nockmaRunEphemeralArgs <- optional anomaArgsOpt
  _nockmaRunEphemeralAnomaDir <- anomaDirOpt
  pure NockmaRunEphemeralClientOptions {..}
