module Commands.Termination where

import Control.Monad.Extra
import Data.Text qualified as Text
import GlobalOptions
import Juvix.Prelude hiding (Doc)
import Juvix.Syntax.Abstract.Pretty.Base qualified as A
import Options.Applicative

data TerminationCommand
  = Calls CallsOptions
  | CallGraph CallGraphOptions

data CallsOptions = CallsOptions
  { _callsFunctionNameFilter :: Maybe (NonEmpty Text),
    _callsShowDecreasingArgs :: A.ShowDecrArgs
  }

newtype CallGraphOptions = CallGraphOptions
  { _graphFunctionNameFilter :: Maybe (NonEmpty Text)
  }

makeLenses ''CallsOptions
makeLenses ''CallGraphOptions

parseCalls :: Parser CallsOptions
parseCalls = do
  _callsFunctionNameFilter <-
    fmap msum . optional $
      nonEmpty . Text.words
        <$> option
          str
          ( long "function"
              <> short 'f'
              <> metavar "fun1 fun2 ..."
              <> help "Only shows the specified functions"
          )
  _callsShowDecreasingArgs <-
    option
      decrArgsParser
      ( long "show-decreasing-args"
          <> short 'd'
          <> value A.ArgRel
          <> help "possible values: argument, relation, both"
      )
  pure CallsOptions {..}
  where
    decrArgsParser :: ReadM A.ShowDecrArgs
    decrArgsParser = eitherReader $ \s ->
      case map toLower s of
        "argument" -> return A.OnlyArg
        "relation" -> return A.OnlyRel
        "both" -> return A.ArgRel
        _ -> Left "bad argument"

parseCallGraph :: Parser CallGraphOptions
parseCallGraph = do
  _graphFunctionNameFilter <-
    fmap msum . optional $
      nonEmpty . Text.words
        <$> option
          str
          ( long "function"
              <> short 'f'
              <> help "Only shows the specified function"
          )
  pure CallGraphOptions {..}

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

callsPrettyOptions :: GlobalOptions -> CallsOptions -> A.Options
callsPrettyOptions GlobalOptions {..} CallsOptions {..} =
  A.defaultOptions
    { A._optShowNameIds = _globalShowNameIds,
      A._optShowDecreasingArgs = _callsShowDecreasingArgs
    }
