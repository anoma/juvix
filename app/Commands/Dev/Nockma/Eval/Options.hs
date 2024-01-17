module Commands.Dev.Nockma.Eval.Options where

import CommonOptions

newtype NockmaEvalOptions = NockmaEvalOptions
  { _nockmaEvalFile :: AppPath File
  }
  deriving stock (Data)

makeLenses ''NockmaEvalOptions

parseNockmaEvalOptions :: Parser NockmaEvalOptions
parseNockmaEvalOptions = do
  _nockmaEvalFile <- parseInputFile FileExtNockma
  pure NockmaEvalOptions {..}
