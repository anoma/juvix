module Commands.Dev.Core where

import Juvix.Prelude hiding (Doc)
import Options.Applicative

data CoreCommand
  = Repl CoreEvalOptions
  | Eval CoreEvalOptions

newtype CoreEvalOptions = CoreEvalOptions
  { _coreEvalNoIO :: Bool
  }

makeLenses ''CoreEvalOptions

defaultCoreEvalOptions :: CoreEvalOptions
defaultCoreEvalOptions =
  CoreEvalOptions
    { _coreEvalNoIO = False
    }

parseCoreCommand :: Parser CoreCommand
parseCoreCommand =
  hsubparser $
    mconcat
      [ commandRepl,
        commandEval
      ]
  where
    commandRepl :: Mod CommandFields CoreCommand
    commandRepl = command "repl" replInfo

    commandEval :: Mod CommandFields CoreCommand
    commandEval = command "eval" evalInfo

    replInfo :: ParserInfo CoreCommand
    replInfo =
      info
        (Repl <$> parseCoreEvalOptions)
        (progDesc "Start an interactive session of the JuvixCore evaluator")

    evalInfo :: ParserInfo CoreCommand
    evalInfo =
      info
        (Eval <$> parseCoreEvalOptions)
        (progDesc "Evaluate a JuvixCore file and pretty print the result")

parseCoreEvalOptions :: Parser CoreEvalOptions
parseCoreEvalOptions = do
  _coreEvalNoIO <-
    switch
      ( long "no-io"
          <> help "Don't interpret the IO effects"
      )
  pure CoreEvalOptions {..}
