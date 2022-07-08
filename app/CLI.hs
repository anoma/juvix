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

parseCommand :: Parser CLI
parseCommand = Command <$> parseCommandGlobalOptions

parseCLI :: Parser CLI
parseCLI =
  parseDisplayVersion
    <|> parseDisplayHelp
    <|> parseCommand

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
