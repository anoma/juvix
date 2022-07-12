module Command
  ( module Command,
    module Commands.Extra,
    module Commands.Html,
    module Commands.Compile,
    module Commands.Internal,
  )
where

import Commands.Compile
import Commands.Extra
import Commands.Html
import Commands.Internal
import GlobalOptions
import Juvix.Prelude hiding (Doc)
import Options.Applicative

data Command
  = Check
  | Compile CompileOptions
  | Html HtmlOptions
  | Internal InternalCommand

data CommandGlobalOptions = CommandGlobalOptions
  { _cliCommand :: Command,
    _cliGlobalOptions :: GlobalOptions
  }

makeLenses ''CommandGlobalOptions

parseCommandGlobalOptions :: Parser CommandGlobalOptions
parseCommandGlobalOptions = do
  opts <- parseGlobalFlags False
  cmd <-
    hsubparser
      ( mconcat
          [ commandCheck,
            commandCompile,
            commandHtml,
            commandInternal
          ]
      )
  return (cmd {_cliGlobalOptions = opts <> cmd ^. cliGlobalOptions})

commandCheck :: Mod CommandFields CommandGlobalOptions
commandCheck =
  command "typecheck" $
    info
      (addGlobalOptions (pure Check))
      (progDesc "Type check a Juvix file")

commandCompile :: Mod CommandFields CommandGlobalOptions
commandCompile =
  command "compile" $
    info
      (addGlobalOptions (Compile <$> parseCompile))
      (progDesc "Compile a Juvix file")

commandHtml :: Mod CommandFields CommandGlobalOptions
commandHtml =
  command "html" $
    info
      (addGlobalOptions (Html <$> parseHtml))
      (progDesc "Generate HTML for a Juvix file")

commandInternal :: Mod CommandFields CommandGlobalOptions
commandInternal =
  command "internal" $
    info
      (addGlobalOptions (Internal <$> parseInternalCommand))
      (progDesc "Internal subcommands")

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

cmdDefaultOptions :: Command -> CommandGlobalOptions
cmdDefaultOptions _cliCommand =
  CommandGlobalOptions {_cliGlobalOptions = mempty, ..}

liftParserCmd :: Parser Command -> Parser CommandGlobalOptions
liftParserCmd cmd = cmdDefaultOptions <$> cmd

addGlobalOptions :: Parser Command -> Parser CommandGlobalOptions
addGlobalOptions parser = do
  flags1 <- parseGlobalFlags True
  ~(opts2, _cliCommand) <- addParser (parseGlobalFlags True) parser
  fs <- parserInputFiles
  return
    CommandGlobalOptions
      { _cliGlobalOptions = flags1 <> opts2 <> mempty {_globalInputFiles = fs},
        ..
      }
