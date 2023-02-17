module TopCommand.Options where

import Commands.Compile.Options
import Commands.Dev.Options qualified as Dev
import Commands.Doctor.Options
import Commands.Eval.Options
import Commands.Html.Options
import Commands.Repl.Options
import Commands.Typecheck.Options
import CommonOptions hiding (Doc)
import Data.Generics.Uniplate.Data
import GlobalOptions
import Options.Applicative.Help.Pretty

data TopCommand
  = DisplayVersion
  | DisplayHelp
  | Typecheck TypecheckOptions
  | Compile CompileOptions
  | Eval EvalOptions
  | Html HtmlOptions
  | Dev Dev.DevCommand
  | Doctor DoctorOptions
  | Init
  | JuvixRepl ReplOptions
  deriving stock (Data)

topCommandInputFile :: TopCommand -> Maybe (SomeBase File)
topCommandInputFile = firstJust getInputFile . universeBi
  where
    getInputFile :: AppPath File -> Maybe (SomeBase File)
    getInputFile p
      | p ^. pathIsInput = Just (p ^. pathPath)
      | otherwise = Nothing

parseDisplayVersion :: Parser TopCommand
parseDisplayVersion =
  flag'
    DisplayVersion
    ( long "version"
        <> short 'v'
        <> help "Show the version"
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
          commandRepl
        ]
    )
  where
    commandInit :: Mod CommandFields TopCommand
    commandInit =
      command
        "init"
        ( info
            (pure Init)
            (progDesc "Interactively initialize a juvix project in the current directory")
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
      (Compile <$> parseCompileOptions)
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
