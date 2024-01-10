module Commands.Dev.Nockma.Repl.Options where

import CommonOptions

newtype NockmaReplOptions = NockmaReplOptions
  { _nockmaReplOptionsStackFile :: Maybe (AppPath File)
  }
  deriving stock (Data)

parseNockmaReplOptions :: Parser NockmaReplOptions
parseNockmaReplOptions = do
  _nockmaReplOptionsStackFile <- optional (parseInputFile FileExtNockma)
  pure NockmaReplOptions {..}
