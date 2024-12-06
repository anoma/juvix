module Commands.Dev.Anoma.Indexer.Options where

import Commands.Dev.Anoma.Indexer.ListUnrevealedCommits.Options
import CommonOptions

newtype AnomaIndexerCommand
  = AnomaIndexerListUnrevealedCommits IndexerListUnrevealedCommitsOptions
  deriving stock (Data)

parseAnomaIndexerCommand :: Parser AnomaIndexerCommand
parseAnomaIndexerCommand =
  hsubparser commandListUnrevealedCommits
  where
    commandListUnrevealedCommits :: Mod CommandFields AnomaIndexerCommand
    commandListUnrevealedCommits = command "list-unrevealed-commits" runInfo
      where
        runInfo :: ParserInfo AnomaIndexerCommand
        runInfo =
          info
            (AnomaIndexerListUnrevealedCommits <$> parseUnrevealedCommitsOptions)
            (progDesc "Call the Anoma.Protobuf.IndexerService.ListUnrevealedCommits endpoint")
