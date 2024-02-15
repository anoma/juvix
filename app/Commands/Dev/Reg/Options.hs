module Commands.Dev.Reg.Options where

import Commands.Dev.Reg.Read.Options
import Commands.Dev.Reg.Run.Options
import CommonOptions

data RegCommand
  = Run RegRunOptions
  | Read RegReadOptions
  deriving stock (Data)

parseRegCommand :: Parser RegCommand
parseRegCommand =
  hsubparser $
    mconcat
      [ commandRun,
        commandRead
      ]
  where
    commandRun :: Mod CommandFields RegCommand
    commandRun = command "run" runInfo

    commandRead :: Mod CommandFields RegCommand
    commandRead = command "read" readInfo

    runInfo :: ParserInfo RegCommand
    runInfo =
      info
        (Run <$> parseRegRunOptions)
        (progDesc "Run a JuvixReg file")

    readInfo :: ParserInfo RegCommand
    readInfo =
      info
        (Read <$> parseRegReadOptions)
        (progDesc "Parse a JuvixReg file and pretty print it")
