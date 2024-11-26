module Commands.Dev.Anoma.AddTransaction.Options where

import CommonOptions

data AddTransactionOptions = AddTransactionOptions
  { _addTransactionFile :: AppPath File,
    _addTransactionClientInfo :: Maybe (AppPath File)
  }
  deriving stock (Data)

makeLenses ''AddTransactionOptions

parseAddTransactionOptions :: Parser AddTransactionOptions
parseAddTransactionOptions = do
  _addTransactionFile <- parseInputFile FileExtNockma
  _addTransactionClientInfo <- optional anomaClientConfigOpt
  pure AddTransactionOptions {..}
