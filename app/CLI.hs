module CLI
  ( module CLI,
    module GlobalOptions,
    module Command,
  )
where

import Command
import GlobalOptions
import Juvix.Prelude hiding (Doc)
import Options.Applicative
import Options.Applicative.Builder.Internal
import Options.Applicative.Help.Pretty

data CLI
  = DisplayVersion
  | DisplayHelp
  | Command CommandGlobalOptions
  | Doctor DoctorOptions
  | Init

parseDisplayVersion :: Parser CLI
parseDisplayVersion =
  flag'
    DisplayVersion
    (long "version" <> short 'v' <> help "Show the version" <> noGlobal)

parseDisplayHelp :: Parser CLI
parseDisplayHelp =
  flag'
    DisplayHelp
    (long "help" <> short 'h' <> help "Show the help text" <> noGlobal)

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
            (progDesc "Initialize a juvix project in the current directory")
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
parseCompiler = Command <$> parseCommandGlobalOptions

parseCLI :: Parser CLI
parseCLI =
  parseDisplayVersion
    <|> parseDisplayHelp
    <|> parseCompiler
    <|> parseUtility

commandFirstFile :: CommandGlobalOptions -> Maybe FilePath
commandFirstFile CommandGlobalOptions {_cliGlobalOptions = GlobalOptions {..}} =
  listToMaybe _globalInputFiles

makeAbsPaths :: CLI -> IO CLI
makeAbsPaths cli = case cli of
  Command cmd -> do
    nOpts <- traverseOf globalInputFiles (mapM canonicalizePath) (cmd ^. cliGlobalOptions)
    return (Command (set cliGlobalOptions nOpts cmd))
  _ -> return cli

descr :: ParserInfo CLI
descr =
  info
    parseCLI
    ( fullDesc
        <> progDesc "The Juvix compiler."
        <> footerDoc (Just foot)
    )
  where
    foot :: Doc
    foot = bold "maintainers: " <> "The Juvix Team"
