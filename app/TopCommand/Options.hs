module TopCommand.Options where

import Commands.Compile.Options
import Commands.Dev.Options qualified as Dev
import Commands.Doctor.Options
import Commands.Eval.Options
import Commands.Format.Options
import Commands.Html.Options
import Commands.Repl.Options
import Commands.Typecheck.Options
import CommonOptions hiding (Doc)
import Data.Generics.Uniplate.Data
import GlobalOptions
import Options.Applicative.Help.Pretty

data TopCommand
  = DisplayVersion
  | DisplayNumericVersion
  | DisplayHelp
  | Typecheck TypecheckOptions
  | Compile CompileOptions
  | Eval EvalOptions
  | Html HtmlOptions
  | Dev Dev.DevCommand
  | Doctor DoctorOptions
  | Init
  | JuvixRepl ReplOptions
  | JuvixFormat FormatOptions
  deriving stock (Data)

topCommandInputFile :: TopCommand -> IO (Maybe (Path Abs Dir))
topCommandInputFile t = do
  d <- firstJustM getInputFileOrDir (universeBi t)
  f <- firstJustM getInputFileDir (universeBi t)
  return (f <|> d)
  where
    getInputFileDir :: AppPath File -> IO (Maybe (Path Abs Dir))
    getInputFileDir p
      | p ^. pathIsInput = do
          cwd <- getCurrentDir
          Just . parent <$> prepathToAbsFile cwd (p ^. pathPath)
      | otherwise = return Nothing

    getInputFileOrDir :: AppPath FileOrDir -> IO (Maybe (Path Abs Dir))
    getInputFileOrDir p
      | p ^. pathIsInput = do
          cwd <- getCurrentDir
          lr <- fromPreFileOrDir cwd (p ^. pathPath)
          return . Just $ case lr of
            Left file -> parent file
            Right dir -> dir
      | otherwise = return Nothing

parseDisplayVersion :: Parser TopCommand
parseDisplayVersion =
  flag'
    DisplayVersion
    ( long "version"
        <> short 'v'
        <> help "Show version information"
    )

parseDisplayNumericVersion :: Parser TopCommand
parseDisplayNumericVersion =
  flag'
    DisplayNumericVersion
    ( long "numeric-version"
        <> help "Show only the version number"
    )

parseDisplayHelp :: Parser TopCommand
parseDisplayHelp =
  flag'
    DisplayHelp
    ( long "help"
        <> short 'h'
        <> help "Show the help text"
    )

parseUtility :: Parser TopCommand
parseUtility =
  hsubparser
    ( mconcat
        [ commandGroup "Utility commands:",
          metavar "UTILITY_CMD",
          commandDoctor,
          commandInit,
          commandDev,
          commandRepl,
          commandFormat
        ]
    )
  where
    commandInit :: Mod CommandFields TopCommand
    commandInit =
      command
        "init"
        ( info
            (pure Init)
            (progDesc "Interactively initialize a Juvix project in the current directory")
        )
    commandDoctor :: Mod CommandFields TopCommand
    commandDoctor =
      command
        "doctor"
        ( info
            (Doctor <$> parseDoctorOptions)
            (progDesc "Perform checks on your Juvix development environment")
        )
    commandRepl :: Mod CommandFields TopCommand
    commandRepl =
      command
        "repl"
        ( info
            (JuvixRepl <$> parseRepl)
            (progDesc "Run the Juvix REPL")
        )

    commandFormat :: Mod CommandFields TopCommand
    commandFormat =
      command "format" $
        info
          (JuvixFormat <$> parseFormat)
          ( progDescDoc
              ( Just
                  ( vsep
                      [ "Format a Juvix file or Juvix project",
                        "",
                        "When the command is run with an unformatted file it prints the reformatted source to standard output.",
                        "When the command is run with a project directory it prints a list of unformatted files in the project."
                      ]
                  )
              )
          )

commandCheck :: Mod CommandFields TopCommand
commandCheck =
  command "typecheck" $
    info
      (Typecheck <$> parseTypecheck)
      (progDesc "Typecheck a Juvix file")

commandCompile :: Mod CommandFields TopCommand
commandCompile =
  command "compile" $
    info
      (Compile <$> parseMainCompileOptions)
      (progDesc "Compile a Juvix file")

commandEval :: Mod CommandFields TopCommand
commandEval =
  command "eval" $
    info
      (Eval <$> parseEvalOptions)
      (progDesc "Evaluate a Juvix file")

commandHtml :: Mod CommandFields TopCommand
commandHtml =
  command "html" $
    info
      (Html <$> parseHtml)
      (progDesc "Generate HTML for a Juvix file")

commandDev :: Mod CommandFields TopCommand
commandDev =
  command "dev" $
    info
      (Dev <$> Dev.parseDevCommand)
      (progDesc "Commands for the developers")

parseCompilerCommand :: Parser TopCommand
parseCompilerCommand =
  hsubparser
    ( mconcat
        [ commandGroup "Compiler commands:",
          metavar "COMPILER_CMD",
          commandCheck,
          commandCompile,
          commandEval,
          commandHtml
        ]
    )

parseTopCommand :: Parser TopCommand
parseTopCommand =
  parseDisplayVersion
    <|> parseDisplayNumericVersion
    <|> parseDisplayHelp
    <|> parseCompilerCommand
    <|> parseUtility

descr :: ParserInfo (GlobalOptions, TopCommand)
descr =
  info
    ( do
        cli <- parseTopCommand
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
