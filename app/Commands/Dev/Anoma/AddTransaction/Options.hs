module Commands.Dev.Anoma.AddTransaction.Options where

import CommonOptions

data AddTransactionOptions = AddTransactionOptions
  { _addTransactionFile :: AppPath File,
    _addTransactionShielded :: Bool
  }
  deriving stock (Data)

makeLenses ''AddTransactionOptions

parseAddTransactionOptions :: Parser AddTransactionOptions
parseAddTransactionOptions = do
  _addTransactionFile <- parseInputFile FileExtNockma
  _addTransactionShielded <- switch (long "shielded" <> help "Add a shielded transaction")
  pure AddTransactionOptions {..}
