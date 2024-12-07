module Commands.Dev.Anoma.Indexer.ListUnrevealedCommits.Options where

import CommonOptions

newtype IndexerListUnrevealedCommitsOptions = IndexerListUnrevealedCommitsOptions
  {_indexerListUnrevealedCommitsOutputFile :: Maybe (AppPath File)}
  deriving stock (Data)

parseUnrevealedCommitsOptions :: Parser IndexerListUnrevealedCommitsOptions
parseUnrevealedCommitsOptions = do
  _indexerListUnrevealedCommitsOutputFile <- optional parseGenericOutputFile
  pure IndexerListUnrevealedCommitsOptions {..}

makeLenses ''IndexerListUnrevealedCommitsOptions
