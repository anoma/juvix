module Commands.Dev.Internal where

import Juvix.Prelude hiding (Doc)
import Options.Applicative

data MicroCommand
  = Pretty
  | TypeCheck InternalTypeOptions
  | Arity

newtype InternalTypeOptions = InternalTypeOptions
  { _microJuvixTypePrint :: Bool
  }

makeLenses ''InternalTypeOptions

defaultInternalTypeOptions :: InternalTypeOptions
defaultInternalTypeOptions =
  InternalTypeOptions
    { _microJuvixTypePrint = False
    }

instance Semigroup InternalTypeOptions where
  o1 <> o2 =
    InternalTypeOptions
      { _microJuvixTypePrint = (o1 ^. microJuvixTypePrint) || (o2 ^. microJuvixTypePrint)
      }

instance Monoid InternalTypeOptions where
  mempty = defaultInternalTypeOptions
  mappend = (<>)

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

parseInternalType :: Parser InternalTypeOptions
parseInternalType = do
  _microJuvixTypePrint <-
    switch
      ( long "print-result"
          <> help "Print the type checked module if successful"
      )
  pure InternalTypeOptions {..}
