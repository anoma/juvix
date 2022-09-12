module Commands.Dev.Options
  ( module Commands.Dev.Options,
    module Commands.Dev.Core.Options,
    module Commands.Dev.Internal,
    module Commands.Dev.Parse.Options,
    module Commands.Dev.Highlight.Options,
    module Commands.Dev.Scope.Options,
    module Commands.Dev.Doc.Options,
    module Commands.Dev.Termination.Options,
  )
where

import Commands.Dev.Core.Options
import Commands.Dev.Doc.Options
import Commands.Dev.Highlight.Options
import Commands.Dev.Internal
import Commands.Dev.Parse.Options
import Commands.Dev.Scope.Options
import Commands.Dev.Termination.Options
import Juvix.Prelude
import Options.Applicative

data DevCommand
  = DisplayRoot
  | Highlight HighlightOptions
  | Internal MicroCommand
  | Core CoreCommand
  | MiniC
  | Parse ParseOptions
  | Scope ScopeOptions
  | Termination TerminationCommand
  | Doc DocOptions

parseDevCommand :: Parser DevCommand
parseDevCommand =
  hsubparser
    ( mconcat
        [ commandHighlight,
          commandInternal,
          commandCore,
          commandMiniC,
          commandParse,
          commandDoc,
          commandScope,
          commandShowRoot,
          commandTermination
        ]
    )

commandDoc :: Mod CommandFields DevCommand
commandDoc =
  command "doc" $
    info
      (Doc <$> parseDoc)
      (progDesc "Generate documentation")

commandHighlight :: Mod CommandFields DevCommand
commandHighlight =
  command "highlight" $
    info
      (Highlight <$> parseHighlight)
      (progDesc "Highlight a Juvix file")

commandMiniC :: Mod CommandFields DevCommand
commandMiniC =
  command "minic" $
    info
      (pure MiniC)
      (progDesc "Translate a Juvix file to MiniC")

commandInternal :: Mod CommandFields DevCommand
commandInternal =
  command "internal" $
    info
      (Internal <$> parseMicroCommand)
      (progDesc "Subcommands related to Internal")

commandCore :: Mod CommandFields DevCommand
commandCore =
  command "core" $
    info
      (Core <$> parseCoreCommand)
      (progDesc "Subcommands related to JuvixCore")

commandParse :: Mod CommandFields DevCommand
commandParse =
  command "parse" $
    info
      (Parse <$> parseParse)
      (progDesc "Parse a Juvix file")

commandScope :: Mod CommandFields DevCommand
commandScope =
  command "scope" $
    info
      (Scope <$> parseScope)
      (progDesc "Parse and scope a Juvix file")

commandShowRoot :: Mod CommandFields DevCommand
commandShowRoot =
  command "root" $
    info
      (pure DisplayRoot)
      (progDesc "Show the root path for a Juvix project")

commandTermination :: Mod CommandFields DevCommand
commandTermination =
  command "termination" $
    info
      (Termination <$> parseTerminationCommand)
      (progDesc "Subcommands related to termination checking")
