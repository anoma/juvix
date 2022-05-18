{-# LANGUAGE ApplicativeDo #-}

module Command
  ( module Command,
    module Commands.Extra,
    module Commands.Html,
    module Commands.MicroJuvix,
    module Commands.MiniC,
    module Commands.MiniHaskell,
    module Commands.MonoJuvix,
    module Commands.Parse,
    module Commands.Scope,
    module Commands.Termination,
  )
where

import Commands.Extra
import Commands.Html
import Commands.MicroJuvix
import Commands.MiniC
import Commands.MiniHaskell
import Commands.MonoJuvix
import Commands.Parse
import Commands.Scope
import Commands.Termination
import GlobalOptions
import MiniJuvix.Prelude hiding (Doc)
import MiniJuvix.Syntax.Concrete.Scoped.Pretty qualified as Scoper
import Options.Applicative

data Command
  = Scope ScopeOptions
  | Parse ParseOptions
  | Html HtmlOptions
  | Termination TerminationCommand
  | MiniHaskell MiniHaskellOptions
  | MiniC MiniCOptions
  | MicroJuvix MicroJuvixCommand
  | MonoJuvix MonoJuvixOptions
  | DisplayVersion
  | DisplayRoot
  | Highlight HighlightOptions

newtype HighlightOptions = HighlightOptions
  { _highlightInputFile :: FilePath
  }

makeLenses ''HighlightOptions

parseHighlight :: Parser HighlightOptions
parseHighlight = do
  _highlightInputFile <- parserInputFile
  pure HighlightOptions {..}

parseDisplayVersion :: Parser Command
parseDisplayVersion =
  flag'
    DisplayVersion
    (long "version" <> short 'v' <> help "Print the version and exit")

parseDisplayRoot :: Parser Command
parseDisplayRoot =
  flag'
    DisplayRoot
    (long "show-root" <> help "Print the detected root of the project")

mkScopePrettyOptions :: GlobalOptions -> ScopeOptions -> Scoper.Options
mkScopePrettyOptions g ScopeOptions {..} =
  Scoper.defaultOptions
    { Scoper._optShowNameId = g ^. globalShowNameIds,
      Scoper._optInlineImports = _scopeInlineImports
    }

parseCommand :: Parser Command
parseCommand =
  parseDisplayVersion
    <|> parseDisplayRoot
    <|> hsubparser
      ( mconcat
          [ commandParse,
            commandScope,
            commandHtml,
            commandTermination,
            commandMonoJuvix,
            commandMicroJuvix,
            commandMiniHaskell,
            commandMiniC,
            commandHighlight
          ]
      )
  where
    commandMicroJuvix :: Mod CommandFields Command
    commandMicroJuvix = command "microjuvix" minfo
      where
        minfo :: ParserInfo Command
        minfo =
          info
            (MicroJuvix <$> parseMicroJuvixCommand)
            (progDesc "Subcommands related to MicroJuvix")

    commandMonoJuvix :: Mod CommandFields Command
    commandMonoJuvix = command "monojuvix" minfo
      where
        minfo :: ParserInfo Command
        minfo =
          info
            (MonoJuvix <$> parseMonoJuvix)
            (progDesc "Translate a MiniJuvix file to MonoJuvix")

    commandMiniHaskell :: Mod CommandFields Command
    commandMiniHaskell = command "minihaskell" minfo
      where
        minfo :: ParserInfo Command
        minfo =
          info
            (MiniHaskell <$> parseMiniHaskell)
            (progDesc "Translate a MiniJuvix file to MiniHaskell")

    commandMiniC :: Mod CommandFields Command
    commandMiniC = command "minic" minfo
      where
        minfo :: ParserInfo Command
        minfo =
          info
            (MiniC <$> parseMiniC)
            (progDesc "Translate a MiniJuvix file to MiniC")

    commandHighlight :: Mod CommandFields Command
    commandHighlight = command "highlight" minfo
      where
        minfo :: ParserInfo Command
        minfo =
          info
            (Highlight <$> parseHighlight)
            (progDesc "Highlight a MiniJuvix file")

    commandParse :: Mod CommandFields Command
    commandParse = command "parse" minfo
      where
        minfo :: ParserInfo Command
        minfo =
          info
            (Parse <$> parseParse)
            (progDesc "Parse a MiniJuvix file")

    commandHtml :: Mod CommandFields Command
    commandHtml = command "html" minfo
      where
        minfo :: ParserInfo Command
        minfo =
          info
            (Html <$> parseHtml)
            (progDesc "Generate HTML for a MiniJuvix file")

    commandScope :: Mod CommandFields Command
    commandScope = command "scope" minfo
      where
        minfo :: ParserInfo Command
        minfo =
          info
            (Scope <$> parseScope)
            (progDesc "Parse and scope a MiniJuvix file")

    commandTermination :: Mod CommandFields Command
    commandTermination = command "termination" minfo
      where
        minfo :: ParserInfo Command
        minfo =
          info
            (Termination <$> parseTerminationCommand)
            (progDesc "Subcommands related to termination checking")
