{-# LANGUAGE ApplicativeDo #-}

module Commands.MicroJuvix where

import Commands.Extra
import MiniJuvix.Prelude hiding (Doc)
import Options.Applicative

data MicroJuvixCommand
  = Pretty MicroJuvixOptions
  | TypeCheck MicroJuvixOptions

newtype MicroJuvixOptions = MicroJuvixOptions
  { _mjuvixInputFile :: FilePath
  }

parseMicroJuvixCommand :: Parser MicroJuvixCommand
parseMicroJuvixCommand =
  hsubparser $
    mconcat
      [ commandPretty
      , commandTypeCheck
      ]
  where
    commandPretty :: Mod CommandFields MicroJuvixCommand
    commandPretty = command "pretty" prettyInfo

    commandTypeCheck :: Mod CommandFields MicroJuvixCommand
    commandTypeCheck = command "typecheck" typeCheckInfo

    prettyInfo :: ParserInfo MicroJuvixCommand
    prettyInfo =
      info
        (Pretty <$> parseMicroJuvix)
        (progDesc "Translate a MiniJuvix file to MicroJuvix and pretty print the result")

    typeCheckInfo :: ParserInfo MicroJuvixCommand
    typeCheckInfo =
      info
        (TypeCheck <$> parseMicroJuvix)
        (progDesc "Translate a MiniJuvix file to MicroJuvix and typecheck the result")

parseMicroJuvix :: Parser MicroJuvixOptions
parseMicroJuvix = do
  _mjuvixInputFile <- parseInputFile
  pure MicroJuvixOptions {..}
