module Commands.Dev.Asm.Options where

import Commands.Dev.Asm.Run.Options
import Commands.Dev.Asm.Validate.Options
import CommonOptions

data AsmCommand
  = Run AsmRunOptions
  | Validate AsmValidateOptions
  deriving stock (Data)

parseAsmCommand :: Parser AsmCommand
parseAsmCommand =
  hsubparser $
    mconcat [
      commandRun,
      commandValidate
    ]
  where
    commandRun :: Mod CommandFields AsmCommand
    commandRun = command "run" runInfo

    commandValidate :: Mod CommandFields AsmCommand
    commandValidate = command "validate" validateInfo

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

