module Commands.Dev.Anoma.AddTransaction.Options where

import CommonOptions

newtype AddTransactionOptions = AddTransactionOptions
  { _addTransactionFile :: AppPath File
  }
  deriving stock (Data)

makeLenses ''AddTransactionOptions

parseAddTransactionOptions :: Parser AddTransactionOptions
parseAddTransactionOptions = do
  _addTransactionFile <- parseInputFile FileExtNockma
  pure AddTransactionOptions {..}
