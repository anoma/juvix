module Commands.Dev.Nockma.Run.BuiltinClient.Options where

import CommonOptions

data NockmaRunBuiltinClientOptions = NockmaRunBuiltinClientOptions
  { _nockmaRunBuiltinFile :: AppPath File,
    _nockmaRunBuiltinProfile :: Bool,
    _nockmaRunBuiltinArgs :: Maybe (AppPath File)
  }
  deriving stock (Data)

makeLenses ''NockmaRunBuiltinClientOptions

parseNockmaRunBuiltinClientOptions :: Parser NockmaRunBuiltinClientOptions
parseNockmaRunBuiltinClientOptions = do
  _nockmaRunBuiltinFile <- parseInputFile FileExtNockma
  _nockmaRunBuiltinArgs <- optional anomaArgsOpt
  _nockmaRunBuiltinProfile <-
    switch
      ( long "profile"
          <> help "Report evaluator profiling statistics"
      )
  pure NockmaRunBuiltinClientOptions {..}
