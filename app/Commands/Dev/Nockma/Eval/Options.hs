module Commands.Dev.Nockma.Eval.Options where

import CommonOptions

data NockmaEvalOptions = NockmaEvalOptions
  { _nockmaEvalFile :: AppPath File,
    _nockmaEvalProfile :: Bool
  }
  deriving stock (Data)

makeLenses ''NockmaEvalOptions

parseNockmaEvalOptions :: Parser NockmaEvalOptions
parseNockmaEvalOptions = do
  _nockmaEvalFile <- parseInputFile FileExtNockma
  _nockmaEvalProfile <-
    switch
      ( long "profile"
          <> help "Report evaluator profiling statistics"
      )
  pure NockmaEvalOptions {..}
