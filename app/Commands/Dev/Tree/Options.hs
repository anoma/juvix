module Commands.Dev.Tree.Options where

import Commands.Dev.Tree.Eval.Options
import Commands.Dev.Tree.FromAsm.Options
import Commands.Dev.Tree.Read.Options
import CommonOptions

data TreeCommand
  = Eval TreeEvalOptions
  | Read TreeReadOptions
  | FromAsm TreeFromAsmOptions
  deriving stock (Data)

parseTreeCommand :: Parser TreeCommand
parseTreeCommand =
  hsubparser $
    mconcat
      [ commandEval,
        commandRead,
        commandFromAsm
      ]
  where
    commandEval :: Mod CommandFields TreeCommand
    commandEval = command "eval" evalInfo

    commandRead :: Mod CommandFields TreeCommand
    commandRead = command "read" readInfo

    commandFromAsm :: Mod CommandFields TreeCommand
    commandFromAsm = command "from-asm" fromAsmInfo

    evalInfo :: ParserInfo TreeCommand
    evalInfo =
      info
        (Eval <$> parseTreeEvalOptions)
        (progDesc "Evaluate a JuvixTree file")

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
