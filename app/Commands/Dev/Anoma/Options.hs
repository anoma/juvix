module Commands.Dev.Anoma.Options where

import Commands.Dev.Anoma.Prove.Options
import Commands.Dev.Anoma.Start.Options
import CommonOptions

data AnomaCommand
  = AnomaCommandStart StartOptions
  | AnomaCommandStatus
  | AnomaCommandStop
  | AnomaCommandProve ProveOptions
  deriving stock (Data)

parseAnomaCommand :: Parser AnomaCommand
parseAnomaCommand =
  hsubparser
    ( mconcat
        [ commandStart,
          commandStatus,
          commandStop,
          commandProve
        ]
    )
  where
    commandStart :: Mod CommandFields AnomaCommand
    commandStart = command "start" runInfo
      where
        runInfo :: ParserInfo AnomaCommand
        runInfo =
          info
            (AnomaCommandStart <$> parseStartOptions)
            (progDesc "Start an Anoma client")

    commandStatus :: Mod CommandFields AnomaCommand
    commandStatus = command "status" runInfo
      where
        runInfo :: ParserInfo AnomaCommand
        runInfo =
          info
            (pure AnomaCommandStatus)
            (progDesc "Show the status of the Anoma client")

    commandStop :: Mod CommandFields AnomaCommand
    commandStop = command "stop" runInfo
      where
        runInfo :: ParserInfo AnomaCommand
        runInfo =
          info
            (pure AnomaCommandStop)
            (progDesc "Stop the Anoma client")

    commandProve :: Mod CommandFields AnomaCommand
    commandProve = command "prove" runInfo
      where
        runInfo :: ParserInfo AnomaCommand
        runInfo =
          info
            (AnomaCommandProve <$> parseProveOptions)
            (progDesc "Submit an Anoma program to Anoma.Protobuf.NockService.Prove")
