module Commands.Dev.Nockma.Run.Options where

import CommonOptions

data NockmaRunOptions = NockmaRunOptions
  { _nockmaRunFile :: AppPath File,
    _nockmaEnvFile :: AppPath File,
    _nockmaRunProfile :: Bool
  }
  deriving stock (Data)

makeLenses ''NockmaRunOptions

parseNockmaRunOptions :: Parser NockmaRunOptions
parseNockmaRunOptions = do
  _nockmaRunFile <- parseInputFile FileExtNockma
  _nockmaEnvFile <- do
    _pathPath <-
      option
        somePreFileOpt
        ( long "env"
            <> metavar "INPUT_ENV_FILE"
            <> help "Path to environment file"
            <> action "file"
        )
    pure AppPath {_pathIsInput = True, ..}
  _nockmaRunProfile <-
    switch
      ( long "profile"
          <> help "Report evaluator profiling statistics"
      )
  pure NockmaRunOptions {..}
