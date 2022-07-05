module Command
  ( module Command,
    module Commands.Extra,
    module Commands.Html,
    module Commands.MicroJuvix,
    module Commands.Parse,
    module Commands.Scope,
    module Commands.Termination,
    module Commands.Compile,
  )
where

import Commands.Compile
import Commands.Extra
import Commands.Html
import Commands.MicroJuvix
import Commands.Parse
import Commands.Scope
import Commands.Termination
import GlobalOptions
import MiniJuvix.Prelude hiding (Doc)
import MiniJuvix.Syntax.Concrete.Scoped.Pretty qualified as Scoper
import Options.Applicative

data Command
  = Compile CompileOptions
  | DisplayRoot
  | Highlight
  | Html HtmlOptions
  | MicroJuvix MicroJuvixCommand
  | MiniC
  | MiniHaskell
  | MonoJuvix
  | Parse ParseOptions
  | Scope ScopeOptions
  | Termination TerminationCommand

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
          [ commandCompile,
            commandHighlight,
            commandHtml,
            commandMicroJuvix,
            commandMiniC,
            commandMonoJuvix,
            commandParse,
            commandScope,
            commandShowRoot,
            commandTermination
          ]
      )
      <|> hsubparser (commandMiniHaskell <> internal)
  return (cmd {_cliGlobalOptions = opts <> cmd ^. cliGlobalOptions})

commandCompile :: Mod CommandFields CommandGlobalOptions
commandCompile =
  command "compile" $
    info
      (addGlobalOptions (Compile <$> parseCompile))
      (progDesc "Compile a MiniJuvix file")

commandHighlight :: Mod CommandFields CommandGlobalOptions
commandHighlight =
  command "highlight" $
    info
      (addGlobalOptions (pure Highlight))
      (progDesc "Highlight a MiniJuvix file")

commandHtml :: Mod CommandFields CommandGlobalOptions
commandHtml =
  command "html" $
    info
      (addGlobalOptions (Html <$> parseHtml))
      (progDesc "Generate HTML for a MiniJuvix file")

commandMiniC :: Mod CommandFields CommandGlobalOptions
commandMiniC =
  command "minic" $
    info
      (addGlobalOptions (pure MiniC))
      (progDesc "Translate a MiniJuvix file to MiniC")

commandMicroJuvix :: Mod CommandFields CommandGlobalOptions
commandMicroJuvix =
  command "microjuvix" $
    info
      (addGlobalOptions (MicroJuvix <$> parseMicroJuvixCommand))
      (progDesc "Subcommands related to MicroJuvix")

commandMiniHaskell :: Mod CommandFields CommandGlobalOptions
commandMiniHaskell =
  command "minihaskell" $
    info
      (addGlobalOptions (pure MiniHaskell))
      (progDesc "Translate a MiniJuvix file to MiniHaskell")

commandMonoJuvix :: Mod CommandFields CommandGlobalOptions
commandMonoJuvix =
  command "monojuvix" $
    info
      (addGlobalOptions (pure MonoJuvix))
      (progDesc "Translate a MiniJuvix file to MonoJuvix")

commandParse :: Mod CommandFields CommandGlobalOptions
commandParse =
  command "parse" $
    info
      (addGlobalOptions (Parse <$> parseParse))
      (progDesc "Parse a MiniJuvix file")

commandScope :: Mod CommandFields CommandGlobalOptions
commandScope =
  command "scope" $
    info
      (addGlobalOptions (Scope <$> parseScope))
      (progDesc "Parse and scope a MiniJuvix file")

commandShowRoot :: Mod CommandFields CommandGlobalOptions
commandShowRoot =
  command "root" $
    info
      (liftParserCmd (pure DisplayRoot))
      (progDesc "Show the root path for a Minijuvix project")

commandTermination :: Mod CommandFields CommandGlobalOptions
commandTermination =
  command "termination" $
    info
      (addGlobalOptions $ Termination <$> parseTerminationCommand)
      (progDesc "Subcommands related to termination checking")

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
  ~(opts2, _cliCommand) <- addParser (parseGlobalOptions True) parser
  fs <- parserInputFiles
  return
    CommandGlobalOptions
      { _cliGlobalOptions = flags1 <> opts2 <> mempty {_globalInputFiles = fs},
        ..
      }

mkScopePrettyOptions :: GlobalOptions -> ScopeOptions -> Scoper.Options
mkScopePrettyOptions g ScopeOptions {..} =
  Scoper.defaultOptions
    { Scoper._optShowNameIds = g ^. globalShowNameIds,
      Scoper._optInlineImports = _scopeInlineImports
    }
