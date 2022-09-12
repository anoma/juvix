module Commands.Dev.Termination.Options
  ( module Commands.Dev.Termination.Options,
    module Commands.Dev.Termination.Calls.Options,
    module Commands.Dev.Termination.CallGraph.Options,
  )
where

import Commands.Dev.Termination.CallGraph.Options
import Commands.Dev.Termination.Calls.Options
import Juvix.Prelude
import Options.Applicative

data TerminationCommand
  = Calls CallsOptions
  | CallGraph CallGraphOptions

parseTerminationCommand :: Parser TerminationCommand
parseTerminationCommand =
  hsubparser $
    mconcat
      [ commandCalls,
        commandGraph
      ]
  where
    commandCalls :: Mod CommandFields TerminationCommand
    commandCalls = command "calls" minfo
      where
        minfo :: ParserInfo TerminationCommand
        minfo =
          info
            (Calls <$> parseCalls)
            (progDesc "Compute the calls table of a .juvix file")
    commandGraph :: Mod CommandFields TerminationCommand
    commandGraph = command "graph" minfo
      where
        minfo :: ParserInfo TerminationCommand
        minfo =
          info
            (CallGraph <$> parseCallGraph)
            (progDesc "Compute the complete call graph of a .juvix file")
