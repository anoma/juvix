module Commands.Dev.Termination.Calls.Options where

import CommonOptions
import Data.Text qualified as Text
import GlobalOptions
import Juvix.Compiler.Internal.Pretty.Options qualified as Internal

data CallsOptions = CallsOptions
  { _callsFunctionNameFilter :: Maybe (NonEmpty Text),
    _callsShowDecreasingArgs :: Internal.ShowDecrArgs,
    _callsInputFile :: AppPath File
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
          <> value Internal.ArgRel
          <> help "possible values: argument, relation, both"
      )
  _callsInputFile <- parseInputJuvixFile
  pure CallsOptions {..}
  where
    decrArgsParser :: ReadM Internal.ShowDecrArgs
    decrArgsParser = eitherReader $ \s ->
      case map toLower s of
        "argument" -> return Internal.OnlyArg
        "relation" -> return Internal.OnlyRel
        "both" -> return Internal.ArgRel
        _ -> Left "bad argument"

instance CanonicalProjection (GlobalOptions, CallsOptions) Internal.Options where
  project (GlobalOptions {..}, CallsOptions {..}) =
    Internal.defaultOptions
      { Internal._optShowNameIds = _globalShowNameIds,
        Internal._optShowDecreasingArgs = _callsShowDecreasingArgs
      }
