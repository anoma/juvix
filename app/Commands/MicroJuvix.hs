{-# LANGUAGE ApplicativeDo #-}

module Commands.MicroJuvix where

import Commands.Extra
import MiniJuvix.Prelude hiding (Doc)
import Options.Applicative

data MicroJuvixCommand
  = Pretty MicroJuvixPrettyOptions
  | TypeCheck MicroJuvixTypeOptions

newtype MicroJuvixPrettyOptions = MicroJuvixPrettyOptions
  { _microJuvixPrettyInputFile :: FilePath
  }

data MicroJuvixTypeOptions = MicroJuvixTypeOptions
  { _microJuvixTypeInputFile :: FilePath,
    _microJuvixTypePrint :: Bool
  }

makeLenses ''MicroJuvixPrettyOptions
makeLenses ''MicroJuvixTypeOptions

parseMicroJuvixCommand :: Parser MicroJuvixCommand
parseMicroJuvixCommand =
  hsubparser $
    mconcat
      [ commandPretty,
        commandTypeCheck
      ]
  where
    commandPretty :: Mod CommandFields MicroJuvixCommand
    commandPretty = command "pretty" prettyInfo

    commandTypeCheck :: Mod CommandFields MicroJuvixCommand
    commandTypeCheck = command "typecheck" typeCheckInfo

    prettyInfo :: ParserInfo MicroJuvixCommand
    prettyInfo =
      info
        (Pretty <$> parseMicroJuvixPretty)
        (progDesc "Translate a MiniJuvix file to MicroJuvix and pretty print the result")

    typeCheckInfo :: ParserInfo MicroJuvixCommand
    typeCheckInfo =
      info
        (TypeCheck <$> parseMicroJuvixType)
        (progDesc "Translate a MiniJuvix file to MicroJuvix and typecheck the result")

parseMicroJuvixPretty :: Parser MicroJuvixPrettyOptions
parseMicroJuvixPretty = do
  _microJuvixPrettyInputFile <- parseInputFile
  pure MicroJuvixPrettyOptions {..}

parseMicroJuvixType :: Parser MicroJuvixTypeOptions
parseMicroJuvixType = do
  _microJuvixTypeInputFile <- parseInputFile
  _microJuvixTypePrint <-
    switch
      ( long "print-result"
          <> help "Print the type checked module if successful"
      )
  pure MicroJuvixTypeOptions {..}
