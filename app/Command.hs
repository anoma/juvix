module Command
  ( module Command,
    module Commands.Extra,
    module Commands.Html.Options,
    module Commands.Compile.Options,
    module Commands.Dev.Options,
    module Commands.Doctor,
  )
where

import Commands.Compile.Options
import Commands.Dev.Options
import Commands.Doctor
import Commands.Extra
import Commands.Html.Options
import Juvix.Prelude hiding (Doc)
import Options.Applicative

data Command
  = Check
  | Compile CompileOptions
  | Html HtmlOptions
  | Dev DevCommand

commandCheck :: Mod CommandFields Command
commandCheck =
  command "typecheck" $
    info
      (pure Check)
      (progDesc "Type check a Juvix file")

commandCompile :: Mod CommandFields Command
commandCompile =
  command "compile" $
    info
      (Compile <$> parseCompile)
      (progDesc "Compile a Juvix file")

commandHtml :: Mod CommandFields Command
commandHtml =
  command "html" $
    info
      (Html <$> parseHtml)
      (progDesc "Generate HTML for a Juvix file")

commandDev :: Mod CommandFields Command
commandDev =
  command "dev" $
    info
      (Dev <$> parseDevCommand)
      (progDesc "Commands for the developers")

parseCompilerCommand :: Parser Command
parseCompilerCommand =
    hsubparser
      ( mconcat
          [ commandGroup "Compiler commands:",
            metavar "COMPILER_CMD",
            commandCheck,
            commandCompile,
            commandHtml,
            commandDev
          ]
      )
