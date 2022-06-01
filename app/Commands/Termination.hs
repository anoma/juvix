module Commands.Termination where

import Commands.Extra
import Control.Monad.Extra
import Data.Text qualified as Text
import MiniJuvix.Prelude hiding (Doc)
import MiniJuvix.Syntax.Abstract.Pretty.Base qualified as A
import Options.Applicative

data TerminationCommand
  = Calls CallsOptions
  | CallGraph CallGraphOptions

data CallsOptions = CallsOptions
  { _callsInputFile :: FilePath,
    _callsShowIds :: Bool,
    _callsFunctionNameFilter :: Maybe (NonEmpty Text),
    _callsShowDecreasingArgs :: A.ShowDecrArgs
  }

data CallGraphOptions = CallGraphOptions
  { _graphInputFile :: FilePath,
    _graphFunctionNameFilter :: Maybe (NonEmpty Text)
  }

makeLenses ''CallsOptions
makeLenses ''CallGraphOptions

parseCalls :: Parser CallsOptions
parseCalls = do
  _callsInputFile <- parserInputFile
  _callsShowIds <-
    switch
      ( long "show-name-ids"
          <> help "Show the unique number of each identifier"
      )
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
  _graphInputFile <- parserInputFile
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
            (progDesc "Compute the calls table of a .mjuvix file")
    commandGraph :: Mod CommandFields TerminationCommand
    commandGraph = command "graph" minfo
      where
        minfo :: ParserInfo TerminationCommand
        minfo =
          info
            (CallGraph <$> parseCallGraph)
            (progDesc "Compute the complete call graph of a .mjuvix file")

callsPrettyOptions :: CallsOptions -> A.Options
callsPrettyOptions CallsOptions {..} =
  A.defaultOptions
    { A._optShowNameId = _callsShowIds,
      A._optShowDecreasingArgs = _callsShowDecreasingArgs
    }
