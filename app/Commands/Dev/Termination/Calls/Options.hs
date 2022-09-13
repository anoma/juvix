module Commands.Dev.Termination.Calls.Options where

import CommonOptions
import Data.Text qualified as Text
import GlobalOptions
import Juvix.Compiler.Abstract.Pretty.Base qualified as Abstract

data CallsOptions = CallsOptions
  { _callsFunctionNameFilter :: Maybe (NonEmpty Text),
    _callsShowDecreasingArgs :: Abstract.ShowDecrArgs,
    _callsInputFile :: Path
  }
  deriving stock (Data)

makeLenses ''CallsOptions

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
  _callsInputFile <- parseInputJuvixFile
  pure CallsOptions {..}
  where
    decrArgsParser :: ReadM Abstract.ShowDecrArgs
    decrArgsParser = eitherReader $ \s ->
      case map toLower s of
        "argument" -> return Abstract.OnlyArg
        "relation" -> return Abstract.OnlyRel
        "both" -> return Abstract.ArgRel
        _ -> Left "bad argument"

instance CanonicalProjection (GlobalOptions, CallsOptions) Abstract.Options where
  project (GlobalOptions {..}, CallsOptions {..}) =
    Abstract.defaultOptions
      { Abstract._optShowNameIds = _globalShowNameIds,
        Abstract._optShowDecreasingArgs = _callsShowDecreasingArgs
      }
