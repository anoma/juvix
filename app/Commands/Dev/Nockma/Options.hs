module Commands.Dev.Nockma.Options where

import Commands.Dev.Nockma.Encode.Options
import Commands.Dev.Nockma.Eval.Options
import Commands.Dev.Nockma.Format.Options
import Commands.Dev.Nockma.Ide.Options
import Commands.Dev.Nockma.Repl.Options
import Commands.Dev.Nockma.Run.Options
import CommonOptions

data NockmaCommand
  = NockmaRepl NockmaReplOptions
  | NockmaEval NockmaEvalOptions
  | NockmaFormat NockmaFormatOptions
  | NockmaRun NockmaRunCommand
  | NockmaEncode NockmaEncodeOptions
  | NockmaIde NockmaIdeCommand
  deriving stock (Data)

parseNockmaCommand :: Parser NockmaCommand
parseNockmaCommand =
  hsubparser $
    mconcat
      [ commandRepl,
        commandFromAsm,
        commandFormat,
        commandEncode,
        commandIde,
        commandRun
      ]
  where
    commandIde :: Mod CommandFields NockmaCommand
    commandIde = command "ide" runInfo
      where
        runInfo :: ParserInfo NockmaCommand
        runInfo =
          info
            (NockmaIde <$> parseNockmaIdeCommand)
            (progDesc "Ide related subcommands")

    commandEncode :: Mod CommandFields NockmaCommand
    commandEncode = command "encode" runInfo
      where
        runInfo :: ParserInfo NockmaCommand
        runInfo =
          info
            (NockmaEncode <$> parseNockmaEncodeOptions)
            (progDesc "Encode and decode nockma terms")

    commandRun :: Mod CommandFields NockmaCommand
    commandRun = command "run" runInfo
      where
        runInfo :: ParserInfo NockmaCommand
        runInfo =
          info
            (NockmaRun <$> parseNockmaRunCommand)
            (progDesc "Subcommands used to run an Anoma program. Use with artefacts obtained from compilation with the anoma target")

    commandFromAsm :: Mod CommandFields NockmaCommand
    commandFromAsm = command "eval" fromAsmInfo
      where
        fromAsmInfo :: ParserInfo NockmaCommand
        fromAsmInfo =
          info
            (NockmaEval <$> parseNockmaEvalOptions)
            (progDesc "Evaluate a nockma file. The file should contain a single nockma cell: [subject formula]")

    commandFormat :: Mod CommandFields NockmaCommand
    commandFormat = command "format" replInfo
      where
        replInfo :: ParserInfo NockmaCommand
        replInfo =
          info
            (NockmaFormat <$> parseNockmaFormatOptions)
            (progDesc "Parses a nockma file and outputs the formatted nockma code")

    commandRepl :: Mod CommandFields NockmaCommand
    commandRepl = command "repl" replInfo
      where
        replInfo :: ParserInfo NockmaCommand
        replInfo =
          info
            (NockmaRepl <$> parseNockmaReplOptions)
            (progDesc "Run the nockma repl")
