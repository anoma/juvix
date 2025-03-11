module Commands.Dev.Options
  ( module Commands.Dev.Options,
    module Commands.Dev.Asm.Options,
    module Commands.Dev.Core.Options,
    module Commands.Dev.Latex.Options,
    module Commands.Dev.Internal.Options,
    module Commands.Dev.Parse.Options,
    module Commands.Dev.Highlight.Options,
    module Commands.Dev.Scope.Options,
    module Commands.Dev.Termination.Options,
    module Commands.Dev.DisplayRoot.Options,
  )
where

import Commands.Dev.Anoma.Options
import Commands.Dev.Asm.Options hiding (Compile)
import Commands.Dev.Casm.Options
import Commands.Dev.Core.Options
import Commands.Dev.DevCompile.Options (DevCompileCommand, parseDevCompileCommand)
import Commands.Dev.DisplayRoot.Options
import Commands.Dev.Highlight.Options
import Commands.Dev.ImportTree.Options
import Commands.Dev.Internal.Options
import Commands.Dev.Latex.Options
import Commands.Dev.MigrateJuvixYaml.Options
import Commands.Dev.Nockma.Options
import Commands.Dev.Parse.Options
import Commands.Dev.PlainMarkdown.Options
import Commands.Dev.Reg.Options
import Commands.Dev.Repl.Options
import Commands.Dev.Runtime.Options
import Commands.Dev.Scope.Options
import Commands.Dev.Termination.Options
import Commands.Dev.Tree.Options
import Commands.Repl.Options
import CommonOptions

data DevCommand
  = DisplayRoot RootOptions
  | ImportTree ImportTreeCommand
  | Latex LatexCommand
  | Highlight HighlightOptions
  | Internal InternalCommand
  | DevCompile DevCompileCommand
  | Core CoreCommand
  | Asm AsmCommand
  | Reg RegCommand
  | Tree TreeCommand
  | PlainMarkdown PlainMarkdownCommand
  | Casm CasmCommand
  | Runtime RuntimeCommand
  | Parse ParseOptions
  | Scope ScopeOptions
  | Termination TerminationCommand
  | JuvixDevRepl ReplOptions
  | MigrateJuvixYaml MigrateJuvixYamlOptions
  | Nockma NockmaCommand
  | Anoma AnomaCommandGlobal
  deriving stock (Data)

parseDevCommand :: Parser DevCommand
parseDevCommand =
  hsubparser
    ( mconcat
        [ commandHighlight,
          commandImportTree,
          commandDevCompile,
          commandInternal,
          commandCore,
          commandAsm,
          commandReg,
          commandTree,
          commandCasm,
          commandRuntime,
          commandParse,
          commandScope,
          commandShowRoot,
          commandTermination,
          commandJuvixDevRepl,
          commandMigrateJuvixYaml,
          commandLatex,
          commandAnoma,
          commandNockma,
          commandPlainMarkdown
        ]
    )

commandPlainMarkdown :: Mod CommandFields DevCommand
commandPlainMarkdown =
  command "plain-markdown" $
    info
      (PlainMarkdown <$> parsePlainMarkdownCommand)
      (progDesc "Subcommands related to Markdown (without Juvix)")

commandLatex :: Mod CommandFields DevCommand
commandLatex =
  command "latex" $
    info
      (Latex <$> parseLatex)
      (progDesc "Subcommands related to LaTeX")

commandImportTree :: Mod CommandFields DevCommand
commandImportTree =
  command "import-tree" $
    info
      (ImportTree <$> parseImportTree)
      (progDesc "Subcommands related to the import dependency tree")

commandDevCompile :: Mod CommandFields DevCommand
commandDevCompile =
  command "compile" $
    info
      (DevCompile <$> parseDevCompileCommand)
      (progDesc "Compile a Juvix file to an internal language")

commandHighlight :: Mod CommandFields DevCommand
commandHighlight =
  command "highlight" $
    info
      (Highlight <$> parseHighlight)
      (progDesc "Highlight a Juvix file")

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

commandReg :: Mod CommandFields DevCommand
commandReg =
  command "reg" $
    info
      (Reg <$> parseRegCommand)
      (progDesc "Subcommands related to JuvixReg")

commandTree :: Mod CommandFields DevCommand
commandTree =
  command "tree" $
    info
      (Tree <$> parseTreeCommand)
      (progDesc "Subcommands related to JuvixTree")

commandCasm :: Mod CommandFields DevCommand
commandCasm =
  command "casm" $
    info
      (Casm <$> parseCasmCommand)
      (progDesc "Subcommands related to Cairo Assembly")

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

commandJuvixDevRepl :: Mod CommandFields DevCommand
commandJuvixDevRepl =
  command
    "repl"
    ( info
        (JuvixDevRepl <$> parseDevRepl)
        (progDesc "Run the Juvix dev REPL")
    )

commandMigrateJuvixYaml :: Mod CommandFields DevCommand
commandMigrateJuvixYaml =
  command "migrate-juvix-yaml" $
    info
      (MigrateJuvixYaml <$> parseMigrateJuvixYaml)
      (progDesc "Migrate juvix.yaml to Package.juvix in the current project")

commandNockma :: Mod CommandFields DevCommand
commandNockma =
  command "nockma" $
    info
      (Nockma <$> parseNockmaCommand)
      (progDesc "Subcommands related to the nockma backend")

commandAnoma :: Mod CommandFields DevCommand
commandAnoma =
  let descr :: (IsString a) => a
      descr = "Subcommands related to the Anoma client"
   in command "anoma" $
        info
          (Anoma <$> parseAnomaCommand)
          ( headerDoc
              ( Just
                  ( vsep
                      [ descr,
                        "",
                        "By default, the gRPC request is made to the client that is started by juvix dev anoma start.",
                        "Use the -c/--config option to use a different Anoma client.",
                        "The config file format is:",
                        "",
                        "url: <ANOMA_CLIENT_URL>",
                        "port: <ANOMA_CLIENT_GRPC_PORT>",
                        "nodeid: <ANOMA_CLIENT_NODE_ID>"
                      ]
                  )
              )
              <> progDesc descr
          )
