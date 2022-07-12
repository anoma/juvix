module Commands.Internal.MicroJuvix where

import Juvix.Prelude hiding (Doc)
import Options.Applicative

data MicroJuvixCommand
  = Pretty
  | TypeCheck MicroJuvixTypeOptions
  | Arity

newtype MicroJuvixTypeOptions = MicroJuvixTypeOptions
  { _microJuvixTypePrint :: Bool
  }

makeLenses ''MicroJuvixTypeOptions

defaultMicroJuvixTypeOptions :: MicroJuvixTypeOptions
defaultMicroJuvixTypeOptions =
  MicroJuvixTypeOptions
    { _microJuvixTypePrint = False
    }

instance Semigroup MicroJuvixTypeOptions where
  o1 <> o2 =
    MicroJuvixTypeOptions
      { _microJuvixTypePrint = (o1 ^. microJuvixTypePrint) || (o2 ^. microJuvixTypePrint)
      }

instance Monoid MicroJuvixTypeOptions where
  mempty = defaultMicroJuvixTypeOptions
  mappend = (<>)

parseMicroJuvixCommand :: Parser MicroJuvixCommand
parseMicroJuvixCommand =
  hsubparser $
    mconcat
      [ commandPretty,
        commandArity,
        commandTypeCheck
      ]
  where
    commandArity :: Mod CommandFields MicroJuvixCommand
    commandArity = command "arity" arityInfo

    commandPretty :: Mod CommandFields MicroJuvixCommand
    commandPretty = command "pretty" prettyInfo

    commandTypeCheck :: Mod CommandFields MicroJuvixCommand
    commandTypeCheck = command "typecheck" typeCheckInfo

    arityInfo :: ParserInfo MicroJuvixCommand
    arityInfo =
      info
        (pure Arity)
        (progDesc "Translate a Juvix file to MicroJuvix and insert holes")

    prettyInfo :: ParserInfo MicroJuvixCommand
    prettyInfo =
      info
        (pure Pretty)
        (progDesc "Translate a Juvix file to MicroJuvix and pretty print the result")

    typeCheckInfo :: ParserInfo MicroJuvixCommand
    typeCheckInfo =
      info
        (TypeCheck <$> parseMicroJuvixType)
        (progDesc "Translate a Juvix file to MicroJuvix and typecheck the result")

parseMicroJuvixType :: Parser MicroJuvixTypeOptions
parseMicroJuvixType = do
  _microJuvixTypePrint <-
    switch
      ( long "print-result"
          <> help "Print the type checked module if successful"
      )
  pure MicroJuvixTypeOptions {..}
