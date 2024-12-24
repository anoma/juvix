module Commands.Dev.Anoma.PrintConfig.Options where

import CommonOptions

data PrintConfigOptions = PrintConfigOptions
  deriving stock (Data)

parsePrintConfigOptions :: Parser PrintConfigOptions
parsePrintConfigOptions = pure PrintConfigOptions
