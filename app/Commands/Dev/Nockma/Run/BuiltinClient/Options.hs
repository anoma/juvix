module Commands.Dev.Nockma.Run.BuiltinClient.Options where

import CommonOptions

data NockmaRunBuiltinClientOptions = NockmaRunBuiltinClientOptions
  { _nockmaRunBuiltinFile :: AppPath File,
    _nockmaRunBuiltinAnomaDir :: Maybe (AppPath Dir),
    _nockmaRunBuiltinProfile :: Bool,
    _nockmaRunBuiltinArgs :: Maybe (AppPath File)
  }
  deriving stock (Data)

makeLenses ''NockmaRunBuiltinClientOptions

parseNockmaRunBuiltinClientOptions :: Parser NockmaRunBuiltinClientOptions
parseNockmaRunBuiltinClientOptions = do
  _nockmaRunBuiltinFile <- parseInputFile FileExtNockma
  _nockmaRunBuiltinArgs <- optional anomaArgsOpt
  _nockmaRunBuiltinAnomaDir <- optional anomaDirOpt
  _nockmaRunBuiltinProfile <-
    switch
      ( long "profile"
          <> help "Report evaluator profiling statistics"
      )
  pure NockmaRunBuiltinClientOptions {..}
