module Commands.Dev.Termination where

import Control.Monad.Extra
import Data.Text qualified as Text
import GlobalOptions
import Juvix.Compiler.Abstract.Pretty.Base qualified as Abstract
import Juvix.Prelude hiding (Doc)
import Options.Applicative

data TerminationCommand
  = Calls CallsOptions
  | CallGraph CallGraphOptions

data CallsOptions = CallsOptions
  { _callsFunctionNameFilter :: Maybe (NonEmpty Text),
    _callsShowDecreasingArgs :: Abstract.ShowDecrArgs
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
          <> value Abstract.ArgRel
          <> help "possible values: argument, relation, both"
      )
  pure CallsOptions {..}
  where
    decrArgsParser :: ReadM Abstract.ShowDecrArgs
    decrArgsParser = eitherReader $ \s ->
      case map toLower s of
        "argument" -> return Abstract.OnlyArg
        "relation" -> return Abstract.OnlyRel
        "both" -> return Abstract.ArgRel
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

instance CanonicalProjection (GlobalOptions, CallsOptions) Abstract.Options where
  project (GlobalOptions {..}, CallsOptions {..}) =
    Abstract.defaultOptions
      { Abstract._optShowNameIds = _globalShowNameIds,
        Abstract._optShowDecreasingArgs = _callsShowDecreasingArgs
      }
