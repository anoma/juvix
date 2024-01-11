module Commands.Dev.Nockma.Options where

import Commands.Dev.Nockma.Repl.Options
import CommonOptions

data NockmaCommand
  = NockmaRepl NockmaReplOptions
  deriving stock (Data)

parseNockmaCommand :: Parser NockmaCommand
parseNockmaCommand = hsubparser commandRepl
  where
    commandRepl :: Mod CommandFields NockmaCommand
    commandRepl = command "repl" replInfo

    replInfo :: ParserInfo NockmaCommand
    replInfo =
      info
        (NockmaRepl <$> parseNockmaReplOptions)
        (progDesc "Run the nockma repl")
