module Commands.Dev.VM.Options where

import Commands.Dev.VM.Compile.Options
import Commands.Dev.VM.Run.Options
import CommonOptions

data VMCommand
  = Run VMRunOptions
  | Compile VMCompileOptions
  deriving stock (Data)

parseVMCommand :: Parser VMCommand
parseVMCommand =
  hsubparser $
    mconcat
      [ commandRun,
        commandCompile
      ]
  where
    commandRun :: Mod CommandFields VMCommand
    commandRun = command "run" runInfo

    commandCompile :: Mod CommandFields VMCommand
    commandCompile = command "compile" compileInfo

    runInfo :: ParserInfo VMCommand
    runInfo =
      info
        (Run <$> parseVMRunOptions)
        (progDesc "Run a JuvixVM source file and print the result")

    compileInfo :: ParserInfo VMCommand
    compileInfo =
      info
        (Compile <$> parseVMCompileOptions)
        (progDesc "Compile a JuvixVM source file to VampIR")
