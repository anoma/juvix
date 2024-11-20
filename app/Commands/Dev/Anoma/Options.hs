module Commands.Dev.Anoma.Options where

import Commands.Dev.Anoma.Start.Options
import CommonOptions

newtype AnomaCommand
  = AnomaCommandStart StartOptions
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
            (AnomaCommandStart <$> parseStartOptions)
            (progDesc "Start an Anoma node and client.")
