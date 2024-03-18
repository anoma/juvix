module Commands.Dev.Nockma.Run.Options where

import CommonOptions

data NockmaRunOptions = NockmaRunOptions
  { _nockmaRunFile :: AppPath File,
    _nockmaRunProfile :: Bool,
    _nockmaRunArgs :: Maybe (AppPath File),
    _nockmaRunEnvFile :: AppPath File
  }
  deriving stock (Data)

makeLenses ''NockmaRunOptions

parseNockmaRunOptions :: Parser NockmaRunOptions
parseNockmaRunOptions = do
  _nockmaRunFile <- parseInputFile FileExtNockma
  _nockmaRunArgs <- optional $ do
    _pathPath <-
      option
        somePreFileOpt
        ( long "args"
            <> metavar "ARGS_FILE"
            <> help "Path to file containing args"
            <> action "file"
        )
    pure AppPath {_pathIsInput = True, ..}
  _nockmaRunEnvFile <- do
    _pathPath <-
      option
        somePreFileOpt
        ( long "env"
            <> metavar "ENV_FILE"
            <> help "Path to file containing Juvix environment"
            <> action "file"
        )
    pure AppPath {_pathIsInput = True, ..}
  _nockmaRunProfile <-
    switch
      ( long "profile"
          <> help "Report evaluator profiling statistics"
      )
  pure NockmaRunOptions {..}
