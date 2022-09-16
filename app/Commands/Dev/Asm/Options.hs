module Commands.Dev.Asm.Options where

import Commands.Dev.Asm.Run.Options
import CommonOptions

newtype AsmCommand
  = Run AsmRunOptions
  deriving stock (Data)

parseAsmCommand :: Parser AsmCommand
parseAsmCommand =
  hsubparser commandRun
  where
    commandRun :: Mod CommandFields AsmCommand
    commandRun = command "run" runInfo

    runInfo :: ParserInfo AsmCommand
    runInfo =
      info
        (Run <$> parseAsmRunOptions)
        (progDesc "Run a JuvixAsm file and pretty print the result")
