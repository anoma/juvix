module Commands.Dev.Options
  ( module Commands.Dev.Options,
    module Commands.Dev.Asm.Options,
    module Commands.Dev.Core.Options,
    module Commands.Dev.Internal.Options,
    module Commands.Dev.Parse.Options,
    module Commands.Dev.Highlight.Options,
    module Commands.Dev.Scope.Options,
    module Commands.Dev.Doc.Options,
    module Commands.Dev.Termination.Options,
    module Commands.Dev.DisplayRoot.Options,
  )
where

import Commands.Dev.Asm.Options hiding (Compile)
import Commands.Dev.Core.Options
import Commands.Dev.DisplayRoot.Options
import Commands.Dev.Doc.Options
import Commands.Dev.Highlight.Options
import Commands.Dev.Internal.Options
import Commands.Dev.MiniC.Options
import Commands.Dev.Parse.Options
import Commands.Dev.Runtime.Options
import Commands.Dev.Scope.Options
import Commands.Dev.Termination.Options
import CommonOptions

data DevCommand
  = DisplayRoot RootOptions
  | Highlight HighlightOptions
  | Internal InternalCommand
  | Core CoreCommand
  | Asm AsmCommand
  | Runtime RuntimeCommand
  | MiniC MiniCOptions
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
          commandAsm,
          commandRuntime,
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
      (MiniC <$> parseMiniC)
      (progDesc "Translate a Juvix file to MiniC")

commandInternal :: Mod CommandFields DevCommand
commandInternal =
  command "internal" $
    info
      (Internal <$> parseInternalCommand)
      (progDesc "Subcommands related to Internal")

commandCore :: Mod CommandFields DevCommand
commandCore =
  command "core" $
    info
      (Core <$> parseCoreCommand)
      (progDesc "Subcommands related to JuvixCore")

commandAsm :: Mod CommandFields DevCommand
commandAsm =
  command "asm" $
    info
      (Asm <$> parseAsmCommand)
      (progDesc "Subcommands related to JuvixAsm")

commandRuntime :: Mod CommandFields DevCommand
commandRuntime =
  command "runtime" $
    info
      (Runtime <$> parseRuntimeCommand)
      (progDesc "Subcommands related to the Juvix runtime")

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
      (DisplayRoot <$> parseRoot)
      (progDesc "Show the root path for a Juvix project")

commandTermination :: Mod CommandFields DevCommand
commandTermination =
  command "termination" $
    info
      (Termination <$> parseTerminationCommand)
      (progDesc "Subcommands related to termination checking")
