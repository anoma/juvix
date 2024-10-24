module Commands.Dev.Anoma.Options where

import Commands.Dev.Anoma.Node.Options
import CommonOptions

newtype AnomaCommand
  = Node NodeOptions
  deriving stock (Data)

parseAnomaCommand :: Parser AnomaCommand
parseAnomaCommand =
  hsubparser
    ( mconcat
        [commandNode]
    )
  where
    commandNode :: Mod CommandFields AnomaCommand
    commandNode = command "node" runInfo
      where
        runInfo :: ParserInfo AnomaCommand
        runInfo =
          info
            (Node <$> parseNodeOptions)
            (progDesc "Run an Anoma node and client.")
