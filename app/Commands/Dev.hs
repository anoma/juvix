module Commands.Dev
  ( module Commands.Dev,
    module Commands.Dev.Core,
    module Commands.Dev.Internal,
    module Commands.Dev.Parse,
    module Commands.Dev.Scope,
    module Commands.Dev.Doc,
    module Commands.Dev.Termination,
  )
where

import Commands.Dev.Core
import Commands.Dev.Doc
import Commands.Dev.Internal
import Commands.Dev.Parse
import Commands.Dev.Scope
import Commands.Dev.Termination
import Juvix.Compiler.Concrete.Data.Highlight
import Juvix.Prelude
import Options.Applicative

data InternalCommand
  = DisplayRoot
  | Highlight HighlightOptions
  | Internal MicroCommand
  | Core CoreCommand
  | MiniC
  | MiniHaskell
  | MonoJuvix
  | Parse ParseOptions
  | Scope ScopeOptions
  | Termination TerminationCommand
  | Doc DocOptions

newtype HighlightOptions = HighlightOptions
  { _highlightBackend :: HighlightBackend
  }

parseInternalCommand :: Parser InternalCommand
parseInternalCommand =
  hsubparser
    ( mconcat
        [ commandHighlight,
          commandInternal,
          commandCore,
          commandMiniC,
          commandMiniHaskell,
          commandMonoJuvix,
          commandParse,
          commandDoc,
          commandScope,
          commandShowRoot,
          commandTermination
        ]
    )

commandDoc :: Mod CommandFields InternalCommand
commandDoc =
  command "doc" $
    info
      (Doc <$> parseDoc)
      (progDesc "Generate documentation")

commandHighlight :: Mod CommandFields InternalCommand
commandHighlight =
  command "highlight" $
    info
      (Highlight <$> parseHighlight)
      (progDesc "Highlight a Juvix file")
  where
    parseHighlight :: Parser HighlightOptions
    parseHighlight = do
      _highlightBackend <-
        option
          (eitherReader parseBackend)
          ( long "format"
              <> metavar "FORMAT"
              <> value Emacs
              <> showDefault
              <> help "selects a backend. FORMAT = emacs | json"
          )
      pure HighlightOptions {..}
    parseBackend :: String -> Either String HighlightBackend
    parseBackend s = case s of
      "emacs" -> Right Emacs
      "json" -> Right Json
      _ -> Left $ "unrecognised theme: " <> s

commandMiniC :: Mod CommandFields InternalCommand
commandMiniC =
  command "minic" $
    info
      (pure MiniC)
      (progDesc "Translate a Juvix file to MiniC")

commandInternal :: Mod CommandFields InternalCommand
commandInternal =
  command "microjuvix" $
    info
      (Internal <$> parseMicroCommand)
      (progDesc "Subcommands related to Internal")

commandCore :: Mod CommandFields InternalCommand
commandCore =
  command "core" $
    info
      (Core <$> parseCoreCommand)
      (progDesc "Subcommands related to JuvixCore")

commandMiniHaskell :: Mod CommandFields InternalCommand
commandMiniHaskell =
  command "minihaskell" $
    info
      (pure MiniHaskell)
      (progDesc "Translate a Juvix file to MiniHaskell")

commandMonoJuvix :: Mod CommandFields InternalCommand
commandMonoJuvix =
  command "monojuvix" $
    info
      (pure MonoJuvix)
      (progDesc "Translate a Juvix file to MonoJuvix")

commandParse :: Mod CommandFields InternalCommand
commandParse =
  command "parse" $
    info
      (Parse <$> parseParse)
      (progDesc "Parse a Juvix file")

commandScope :: Mod CommandFields InternalCommand
commandScope =
  command "scope" $
    info
      (Scope <$> parseScope)
      (progDesc "Parse and scope a Juvix file")

commandShowRoot :: Mod CommandFields InternalCommand
commandShowRoot =
  command "root" $
    info
      (pure DisplayRoot)
      (progDesc "Show the root path for a Juvix project")

commandTermination :: Mod CommandFields InternalCommand
commandTermination =
  command "termination" $
    info
      (Termination <$> parseTerminationCommand)
      (progDesc "Subcommands related to termination checking")
