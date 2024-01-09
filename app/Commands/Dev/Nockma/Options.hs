module Commands.Dev.Nockma.Options where

import Commands.Dev.Nockma.Eval.Options
import Commands.Dev.Nockma.Repl.Options
import CommonOptions

data NockmaCommand
  = NockmaRepl NockmaReplOptions
  | NockmaEval NockmaEvalOptions
  deriving stock (Data)

parseNockmaCommand :: Parser NockmaCommand
parseNockmaCommand =
  hsubparser $
    mconcat
      [ commandRepl,
        commandFromAsm
      ]
  where
    commandFromAsm :: Mod CommandFields NockmaCommand
    commandFromAsm = command "eval" fromAsmInfo
      where
        fromAsmInfo :: ParserInfo NockmaCommand
        fromAsmInfo =
          info
            (NockmaEval <$> parseNockmaEvalOptions)
            (progDesc "Evaluate a nockma file. The file should contain a single nockma cell: [subject formula]")

    commandRepl :: Mod CommandFields NockmaCommand
    commandRepl = command "repl" replInfo
      where
        replInfo :: ParserInfo NockmaCommand
        replInfo =
          info
            (NockmaRepl <$> parseNockmaReplOptions)
            (progDesc "Run the nockma repl")
