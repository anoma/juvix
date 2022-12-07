module Commands.Dev.Asm.Options where

import Commands.Dev.Asm.Compile.Options
import Commands.Dev.Asm.Run.Options
import Commands.Dev.Asm.Validate.Options
import CommonOptions

data AsmCommand
  = Run AsmRunOptions
  | Validate AsmValidateOptions
  | Compile AsmCompileOptions

parseAsmCommand :: Parser AsmCommand
parseAsmCommand =
  hsubparser $
    mconcat
      [ commandRun,
        commandValidate,
        commandCompile
      ]
  where
    commandRun :: Mod CommandFields AsmCommand
    commandRun = command "run" runInfo

    commandValidate :: Mod CommandFields AsmCommand
    commandValidate = command "validate" validateInfo

    commandCompile :: Mod CommandFields AsmCommand
    commandCompile = command "compile" compileInfo

    runInfo :: ParserInfo AsmCommand
    runInfo =
      info
        (Run <$> parseAsmRunOptions)
        (progDesc "Run a JuvixAsm file and pretty print the result")

    validateInfo :: ParserInfo AsmCommand
    validateInfo =
      info
        (Validate <$> parseAsmValidateOptions)
        (progDesc "Validate a JuvixAsm file")

    compileInfo :: ParserInfo AsmCommand
    compileInfo =
      info
        (Compile <$> parseAsmCompileOptions)
        (progDesc "Compile a JuvixAsm file")
