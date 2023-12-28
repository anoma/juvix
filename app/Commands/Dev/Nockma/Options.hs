module Commands.Dev.Nockma.Options where

import Commands.Dev.Nockma.FromAsm.Options
import Commands.Dev.Nockma.Repl.Options
import CommonOptions

data NockmaCommand
  = NockmaRepl NockmaReplOptions
  | NockmaFromAsm NockmaFromAsmOptions
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
    commandFromAsm = command "from-asm" fromAsmInfo
      where
        fromAsmInfo :: ParserInfo NockmaCommand
        fromAsmInfo =
          info
            (NockmaFromAsm <$> parseNockmaFromAsmOptions)
            (progDesc "Read juvix asm and translate it to nockma")

    commandRepl :: Mod CommandFields NockmaCommand
    commandRepl = command "repl" replInfo
      where
        replInfo :: ParserInfo NockmaCommand
        replInfo =
          info
            (NockmaRepl <$> parseNockmaReplOptions)
            (progDesc "Run the nockma repl")
