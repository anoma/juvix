module CLI
  ( module CLI,
    module GlobalOptions,
    module Command,
  )
where

import Command
import CommonOptions hiding (Doc)
import Data.Generics.Uniplate.Data
import GlobalOptions
import Options.Applicative.Help.Pretty

data CLI
  = DisplayVersion
  | DisplayHelp
  | Command Command
  | Doctor DoctorOptions
  | Init
  deriving stock (Data)

parseDisplayVersion :: Parser CLI
parseDisplayVersion =
  flag'
    DisplayVersion
    ( long "version"
        <> short 'v'
        <> help "Show the version"
    )

parseDisplayHelp :: Parser CLI
parseDisplayHelp =
  flag'
    DisplayHelp
    ( long "help"
        <> short 'h'
        <> help "Show the help text"
    )

parseUtility :: Parser CLI
parseUtility =
  hsubparser
    ( mconcat
        [ commandGroup "Utility commands:",
          metavar "UTILITY_CMD",
          cmdDoctor,
          cmdInit
        ]
    )
  where
    cmdInit :: Mod CommandFields CLI
    cmdInit =
      command
        "init"
        ( info
            (pure Init)
            (progDesc "Interactively initialize a juvix project in the current directory")
        )
    cmdDoctor :: Mod CommandFields CLI
    cmdDoctor =
      command
        "doctor"
        ( info
            (Doctor <$> parseDoctorOptions)
            (progDesc "Perform checks on your Juvix development environment")
        )

parseCompiler :: Parser CLI
parseCompiler = Command <$> parseCompilerCommand

parseCLI :: Parser CLI
parseCLI =
  parseDisplayVersion
    <|> parseDisplayHelp
    <|> parseCompiler
    <|> parseUtility

-- TODO: chage 'from' -> 'CLI'
makeAbsPaths :: Biplate from Path => from -> IO from
makeAbsPaths = transformBiM go
  where
    go :: Path -> IO Path
    go = traverseOf unPath canonicalizePath

descr :: ParserInfo (GlobalOptions, CLI)
descr =
  info
    ( do
        cli <- parseCLI
        opts <- parseGlobalFlags
        return (opts, cli)
    )
    ( fullDesc
        <> progDesc "The Juvix compiler."
        <> footerDoc (Just foot)
    )
  where
    foot :: Doc
    foot = bold "maintainers: " <> "The Juvix Team"
