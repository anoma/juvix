module Commands.Dev.VM.Options where

import Commands.Dev.VM.Run.Options
import CommonOptions

newtype VMCommand
  = Run VMRunOptions
  deriving stock (Data)

parseVMCommand :: Parser VMCommand
parseVMCommand =
  hsubparser $
    mconcat
      [ commandRun
      ]
  where
    commandRun :: Mod CommandFields VMCommand
    commandRun = command "run" runInfo

    runInfo :: ParserInfo VMCommand
    runInfo =
      info
        (Run <$> parseVMRunOptions)
        (progDesc "Run a JuvixVM source file and print the result")
