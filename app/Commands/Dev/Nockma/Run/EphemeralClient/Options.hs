module Commands.Dev.Nockma.Run.EphemeralClient.Options where

import Commands.Dev.Anoma.Prove.Options.ProveArg
import CommonOptions

data NockmaRunEphemeralClientOptions = NockmaRunEphemeralClientOptions
  { _nockmaRunEphemeralFile :: AppPath File,
    _nockmaRunEphemeralAnomaDir :: AppPath Dir,
    _nockmaRunEphemeralArgs :: [ProveArg]
  }
  deriving stock (Data)

makeLenses ''NockmaRunEphemeralClientOptions

parseNockmaRunEphemeralClientOptions :: Parser NockmaRunEphemeralClientOptions
parseNockmaRunEphemeralClientOptions = do
  _nockmaRunEphemeralFile <- parseInputFile FileExtNockma
  _nockmaRunEphemeralArgs <- many parseProveArg
  _nockmaRunEphemeralAnomaDir <- anomaDirOpt
  pure NockmaRunEphemeralClientOptions {..}
