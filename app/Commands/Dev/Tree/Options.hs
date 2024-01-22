module Commands.Dev.Tree.Options where

import Commands.Dev.Tree.FromAsm.Options
import Commands.Dev.Tree.Read.Options
import CommonOptions

data TreeCommand
  = Read TreeReadOptions
  | FromAsm TreeFromAsmOptions
  deriving stock (Data)

parseTreeCommand :: Parser TreeCommand
parseTreeCommand =
  hsubparser $
    mconcat
      [ commandRead,
        commandFromAsm
      ]
  where
    commandRead :: Mod CommandFields TreeCommand
    commandRead = command "read" readInfo

    commandFromAsm :: Mod CommandFields TreeCommand
    commandFromAsm = command "from-asm" fromAsmInfo

    readInfo :: ParserInfo TreeCommand
    readInfo =
      info
        (Read <$> parseTreeReadOptions)
        (progDesc "Parse a JuvixTree file and pretty print it")

    fromAsmInfo :: ParserInfo TreeCommand
    fromAsmInfo =
      info
        (FromAsm <$> parseTreeFromAsmOptions)
        (progDesc "Convert a JuvixAsm file to JuvixTree and pretty print it")
