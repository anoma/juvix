module Commands.Dev.Core.Options where

import Commands.Dev.Core.Eval.Options
import Commands.Dev.Core.Read.Options
import Commands.Dev.Core.Repl.Options
import Commands.Dev.Core.Strip.Options
import Commands.Dev.Core.Asm.Options
import CommonOptions

data CoreCommand
  = Repl CoreReplOptions
  | Eval CoreEvalOptions
  | Read CoreReadOptions
  | Strip CoreStripOptions
  | CoreAsm CoreAsmOptions
  deriving stock (Data)

parseCoreCommand :: Parser CoreCommand
parseCoreCommand =
  hsubparser $
    mconcat
      [ commandRepl,
        commandEval,
        commandRead,
        commandStrip,
        commandAsm
      ]
  where
    commandRepl :: Mod CommandFields CoreCommand
    commandRepl = command "repl" replInfo

    commandEval :: Mod CommandFields CoreCommand
    commandEval = command "eval" evalInfo

    commandRead :: Mod CommandFields CoreCommand
    commandRead = command "read" readInfo

    commandStrip :: Mod CommandFields CoreCommand
    commandStrip = command "strip" stripInfo

    commandAsm :: Mod CommandFields CoreCommand
    commandAsm = command "asm" asmInfo

    replInfo :: ParserInfo CoreCommand
    replInfo =
      info
        (Repl <$> parseCoreReplOptions)
        (progDesc "Start an interactive session of the JuvixCore evaluator")

    evalInfo :: ParserInfo CoreCommand
    evalInfo =
      info
        (Eval <$> parseCoreEvalOptions)
        (progDesc "Evaluate a JuvixCore file and pretty print the result")

    readInfo :: ParserInfo CoreCommand
    readInfo =
      info
        (Read <$> parseCoreReadOptions)
        (progDesc "Read a JuvixCore file, transform it, and pretty print it")

    stripInfo :: ParserInfo CoreCommand
    stripInfo =
      info
        (Strip <$> parseCoreStripOptions)
        (progDesc "Translate a JuvixCore file to Core.Stripped and pretty print the result")

    asmInfo :: ParserInfo CoreCommand
    asmInfo =
      info
        (CoreAsm <$> parseCoreAsmOptions)
        (progDesc "Translate a JuvixCore file to JuvixAsm and run the result")
