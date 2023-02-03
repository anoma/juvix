module Commands.Dev.Geb.Options
  ( module Commands.Dev.Geb.Options,
    module Commands.Dev.Geb.Eval.Options,
    module Commands.Dev.Geb.Repl.Options,
  )
where

import Commands.Dev.Geb.Eval.Options
import Commands.Dev.Geb.Read.Options
import Commands.Dev.Geb.Repl.Options
import CommonOptions

data GebCommand
  = GebCommandRepl GebReplOptions
  | GebCommandEval GebEvalOptions
  | GebCommandRead GebReadOptions
  deriving stock (Data)

parseGebCommand :: Parser GebCommand
parseGebCommand =
  hsubparser $
    mconcat
      [ commandRepl,
        commandEval,
        commandRead
      ]
  where
    commandRepl :: Mod CommandFields GebCommand
    commandRepl = command "repl" replInfo

    commandEval :: Mod CommandFields GebCommand
    commandEval = command "eval" evalInfo

    commandRead :: Mod CommandFields GebCommand
    commandRead = command "read" readInfo

    replInfo :: ParserInfo GebCommand
    replInfo =
      info
        (GebCommandRepl <$> parseGebReplOptions)
        (progDesc "Start an interactive session of the JuvixGeb evaluator")

    evalInfo :: ParserInfo GebCommand
    evalInfo =
      info
        (GebCommandEval <$> parseGebEvalOptions)
        (progDesc "Evaluate a JuvixGeb file and pretty print the result")

    readInfo :: ParserInfo GebCommand
    readInfo =
      info
        (GebCommandRead <$> parseGebReadOptions)
        (progDesc "Read a JuvixGeb file and pretty print it")
