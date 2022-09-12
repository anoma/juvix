module Commands.Dev.Internal where

import Commands.Dev.Internal.Typecheck.Options
import Juvix.Prelude hiding (Doc)
import Options.Applicative

data InternalCommand
  = Pretty
  | TypeCheck InternalTypeOptions
  | Arity
  deriving stock (Data)

parseInternalCommand :: Parser InternalCommand
parseInternalCommand =
  hsubparser $
    mconcat
      [ commandPretty,
        commandArity,
        commandTypeCheck
      ]
  where
    commandArity :: Mod CommandFields InternalCommand
    commandArity = command "arity" arityInfo

    commandPretty :: Mod CommandFields InternalCommand
    commandPretty = command "pretty" prettyInfo

    commandTypeCheck :: Mod CommandFields InternalCommand
    commandTypeCheck = command "typecheck" typeCheckInfo

    arityInfo :: ParserInfo InternalCommand
    arityInfo =
      info
        (pure Arity)
        (progDesc "Translate a Juvix file to Internal and insert holes")

    prettyInfo :: ParserInfo InternalCommand
    prettyInfo =
      info
        (pure Pretty)
        (progDesc "Translate a Juvix file to Internal and pretty print the result")

    typeCheckInfo :: ParserInfo InternalCommand
    typeCheckInfo =
      info
        (TypeCheck <$> parseInternalType)
        (progDesc "Translate a Juvix file to Internal and typecheck the result")
