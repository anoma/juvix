module Commands.Dev.Anoma.Intents.Options where

import Commands.Dev.Anoma.Intents.Verify.Options
import CommonOptions

newtype AnomaIntentsCommand
  = AnomaIntentsVerify IntentsVerifyOptions
  deriving stock (Data)

parseAnomaIntentsCommand :: Parser AnomaIntentsCommand
parseAnomaIntentsCommand =
  hsubparser commandVerify
  where
    commandVerify :: Mod CommandFields AnomaIntentsCommand
    commandVerify = command "verify" runInfo
      where
        runInfo :: ParserInfo AnomaIntentsCommand
        runInfo =
          info
            (AnomaIntentsVerify <$> parseIntentsVerifyOptions)
            (progDesc "Call the Anoma.Protobuf.IntentsService.Verify endpoint")
