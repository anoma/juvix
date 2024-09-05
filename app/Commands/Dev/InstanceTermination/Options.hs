module Commands.Dev.InstanceTermination.Options
  ( module Commands.Dev.InstanceTermination.Options,
    module Commands.Dev.InstanceTermination.Calls.Options,
  )
where

import Commands.Dev.InstanceTermination.Calls.Options
import Juvix.Prelude
import Options.Applicative

newtype InstanceTerminationCommand
  = Calls CallsOptions
  deriving stock (Data)

parseInstanceTerminationCommand :: Parser InstanceTerminationCommand
parseInstanceTerminationCommand =
  hsubparser $
    mconcat
      [ commandCalls
      ]
  where
    commandCalls :: Mod CommandFields InstanceTerminationCommand
    commandCalls = command "calls" minfo
      where
        minfo :: ParserInfo InstanceTerminationCommand
        minfo =
          info
            (Calls <$> parseCalls)
            (progDesc "Compute the instance constraints table of a .juvix file")
