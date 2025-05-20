module Commands.Dev.Nockma.Run.BuiltinEvaluator.Options where

import CommonOptions

data NockmaRunBuiltinEvaluatorOptions = NockmaRunBuiltinEvaluatorOptions
  { _nockmaRunBuiltinFile :: AppPath File,
    _nockmaRunBuiltinProfile :: Bool,
    _nockmaRunBuiltinArgs :: Maybe (AppPath File),
    _nockmaRunBuiltinStorage :: Maybe (AppPath File)
  }
  deriving stock (Data)

makeLenses ''NockmaRunBuiltinEvaluatorOptions

parseNockmaRunBuiltinEvaluatorOptions :: Parser NockmaRunBuiltinEvaluatorOptions
parseNockmaRunBuiltinEvaluatorOptions = do
  _nockmaRunBuiltinFile <- parseInputFile FileExtNockma
  _nockmaRunBuiltinArgs <- optional anomaArgsOpt
  _nockmaRunBuiltinProfile <-
    switch
      ( long "profile"
          <> help "Report evaluator profiling statistics"
      )
  _nockmaRunBuiltinStorage <- optional anomaStorageOpt
  pure NockmaRunBuiltinEvaluatorOptions {..}
