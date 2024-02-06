module Commands.Dev.Reg.Options where

import Commands.Dev.Reg.Read.Options
import CommonOptions

newtype RegCommand
  = Read RegReadOptions
  deriving stock (Data)

parseRegCommand :: Parser RegCommand
parseRegCommand =
  hsubparser $
    mconcat
      [ commandRead
      ]
  where
    commandRead :: Mod CommandFields RegCommand
    commandRead = command "read" readInfo

    readInfo :: ParserInfo RegCommand
    readInfo =
      info
        (Read <$> parseRegReadOptions)
        (progDesc "Parse a JuvixReg file and pretty print it")
