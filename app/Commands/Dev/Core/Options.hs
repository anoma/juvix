module Commands.Dev.Core.Options where

import Juvix.Compiler.Core.Data.TransformationId.Parser
import Juvix.Compiler.Core.Pretty.Options qualified as Core
import Juvix.Prelude hiding (Doc)
import Options.Applicative

data CoreCommand
  = Repl CoreReplOptions
  | Eval CoreEvalOptions
  | Read CoreReadOptions

newtype CoreReplOptions = CoreReplOptions
  { _coreReplShowDeBruijn :: Bool
  }

newtype CoreEvalOptions = CoreEvalOptions
  { _coreEvalNoIO :: Bool
  }

data CoreReadOptions = CoreReadOptions
  { _coreReadTransformations :: [TransformationId],
    _coreReadShowDeBruijn :: Bool
  }

makeLenses ''CoreReplOptions
makeLenses ''CoreEvalOptions
makeLenses ''CoreReadOptions

instance CanonicalProjection CoreReplOptions Core.Options where
  project c =
    Core.defaultOptions
      { Core._optShowDeBruijnIndices = c ^. coreReplShowDeBruijn
      }

instance CanonicalProjection CoreReadOptions Core.Options where
  project c =
    Core.defaultOptions
      { Core._optShowDeBruijnIndices = c ^. coreReadShowDeBruijn
      }

defaultCoreEvalOptions :: CoreEvalOptions
defaultCoreEvalOptions =
  CoreEvalOptions
    { _coreEvalNoIO = False
    }

defaultCoreReadOptions :: CoreReadOptions
defaultCoreReadOptions =
  CoreReadOptions
    { _coreReadTransformations = mempty,
      _coreReadShowDeBruijn = False
    }

parseCoreCommand :: Parser CoreCommand
parseCoreCommand =
  hsubparser $
    mconcat
      [ commandRepl,
        commandEval,
        commandRead
      ]
  where
    commandRepl :: Mod CommandFields CoreCommand
    commandRepl = command "repl" replInfo

    commandEval :: Mod CommandFields CoreCommand
    commandEval = command "eval" evalInfo

    commandRead :: Mod CommandFields CoreCommand
    commandRead = command "read" readInfo

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

parseCoreReadOptions :: Parser CoreReadOptions
parseCoreReadOptions = do
  _coreReadShowDeBruijn <- deBruijnOpt
  _coreReadTransformations <-
    option
      (eitherReader parseTransf)
      ( long "transforms"
          <> short 't'
          <> value mempty
          <> metavar "[Transform]"
          <> help "comma sep list of transformations. Available: lifting"
      )
  pure CoreReadOptions {..}
  where
    parseTransf :: String -> Either String [TransformationId]
    parseTransf = mapLeft unpack . parseTransformations . pack

parseCoreEvalOptions :: Parser CoreEvalOptions
parseCoreEvalOptions = do
  _coreEvalNoIO <-
    switch
      ( long "no-io"
          <> help "Don't interpret the IO effects"
      )
  pure CoreEvalOptions {..}

deBruijnOpt :: Parser Bool
deBruijnOpt =
  switch
    ( long "show-de-bruijn"
        <> help "Show variable de Bruijn indices"
    )

parseCoreReplOptions :: Parser CoreReplOptions
parseCoreReplOptions = do
  _coreReplShowDeBruijn <- deBruijnOpt
  pure CoreReplOptions {..}
