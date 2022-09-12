module Commands.Dev.Internal where

import Commands.Dev.Internal.Typecheck.Options
import Juvix.Prelude hiding (Doc)
import Options.Applicative

data MicroCommand
  = Pretty
  | TypeCheck InternalTypeOptions
  | Arity

parseMicroCommand :: Parser MicroCommand
parseMicroCommand =
  hsubparser $
    mconcat
      [ commandPretty,
        commandArity,
        commandTypeCheck
      ]
  where
    commandArity :: Mod CommandFields MicroCommand
    commandArity = command "arity" arityInfo

    commandPretty :: Mod CommandFields MicroCommand
    commandPretty = command "pretty" prettyInfo

    commandTypeCheck :: Mod CommandFields MicroCommand
    commandTypeCheck = command "typecheck" typeCheckInfo

    arityInfo :: ParserInfo MicroCommand
    arityInfo =
      info
        (pure Arity)
        (progDesc "Translate a Juvix file to Internal and insert holes")

    prettyInfo :: ParserInfo MicroCommand
    prettyInfo =
      info
        (pure Pretty)
        (progDesc "Translate a Juvix file to Internal and pretty print the result")

    typeCheckInfo :: ParserInfo MicroCommand
    typeCheckInfo =
      info
        (TypeCheck <$> parseInternalType)
        (progDesc "Translate a Juvix file to Internal and typecheck the result")
