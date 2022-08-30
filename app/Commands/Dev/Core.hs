module Commands.Dev.Core where

import Juvix.Prelude hiding (Doc)
import Options.Applicative

data CoreCommand
  = Repl CoreReplOptions
  | Eval CoreEvalOptions

newtype CoreReplOptions = CoreReplOptions
  { _coreReplShowDeBruijn :: Bool
  }

data CoreEvalOptions = CoreEvalOptions
  { _coreEvalShowDeBruijn :: Bool,
    _coreEvalNoIO :: Bool
  }

makeLenses ''CoreReplOptions
makeLenses ''CoreEvalOptions

defaultCoreEvalOptions :: CoreEvalOptions
defaultCoreEvalOptions =
  CoreEvalOptions
    { _coreEvalShowDeBruijn = False,
      _coreEvalNoIO = False
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
        (Repl <$> parseCoreReplOptions)
        (progDesc "Start an interactive session of the JuvixCore evaluator")

    evalInfo :: ParserInfo CoreCommand
    evalInfo =
      info
        (Eval <$> parseCoreEvalOptions)
        (progDesc "Evaluate a JuvixCore file and pretty print the result")

parseCoreEvalOptions :: Parser CoreEvalOptions
parseCoreEvalOptions = do
  _coreEvalShowDeBruijn <-
    switch
      ( long "show-de-bruijn"
          <> help "Show variable de Bruijn indices"
      )
  _coreEvalNoIO <-
    switch
      ( long "no-io"
          <> help "Don't interpret the IO effects"
      )
  pure CoreEvalOptions {..}

parseCoreReplOptions :: Parser CoreReplOptions
parseCoreReplOptions = do
  _coreReplShowDeBruijn <-
    switch
      ( long "show-de-bruijn"
          <> help "Show variable de Bruijn indices"
      )
  pure CoreReplOptions {..}
