module Commands.Dev.Tree.Options where

import Commands.Dev.Tree.Compile.Options
import Commands.Dev.Tree.Eval.Options
import Commands.Dev.Tree.FromAsm.Options
import Commands.Dev.Tree.Read.Options
import CommonOptions

data TreeCommand
  = Eval TreeEvalOptions
  | Compile CompileOptions
  | Read TreeReadOptions
  | FromAsm TreeFromAsmOptions
  deriving stock (Data)

parseTreeCommand :: Parser TreeCommand
parseTreeCommand =
  hsubparser $
    mconcat
      [ commandEval,
        commandCompile,
        commandRead,
        commandFromAsm
      ]
  where
    commandEval :: Mod CommandFields TreeCommand
    commandEval = command "eval" evalInfo

    commandCompile :: Mod CommandFields TreeCommand
    commandCompile = command "compile" compileInfo

    commandRead :: Mod CommandFields TreeCommand
    commandRead = command "read" readInfo

    commandFromAsm :: Mod CommandFields TreeCommand
    commandFromAsm = command "from-asm" fromAsmInfo

    evalInfo :: ParserInfo TreeCommand
    evalInfo =
      info
        (Eval <$> parseTreeEvalOptions)
        (progDesc "Evaluate a JuvixTree file")

    compileInfo :: ParserInfo TreeCommand
    compileInfo =
      info
        (Compile <$> parseTreeCompileOptions)
        (progDesc "Compile a JuvixTree file")

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
