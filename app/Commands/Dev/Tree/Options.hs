module Commands.Dev.Tree.Options where

import Commands.Dev.Tree.Read.Options
import CommonOptions

newtype TreeCommand
  = Read TreeReadOptions
  deriving stock (Data)

parseTreeCommand :: Parser TreeCommand
parseTreeCommand =
  hsubparser $
    mconcat
      [ commandRead
      ]
  where
    commandRead :: Mod CommandFields TreeCommand
    commandRead = command "read" readInfo

    readInfo :: ParserInfo TreeCommand
    readInfo =
      info
        (Read <$> parseTreeReadOptions)
        (progDesc "Parse a JuvixTree file and pretty print it")
