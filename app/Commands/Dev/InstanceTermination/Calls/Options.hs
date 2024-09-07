module Commands.Dev.InstanceTermination.Calls.Options where

import CommonOptions
import GlobalOptions
import Juvix.Compiler.Internal.Pretty.Options qualified as Internal

data CallsOptions = CallsOptions
  { _callsShowDecreasingArgs :: Internal.ShowDecrArgs,
    _callsInputFile :: AppPath File
  }
  deriving stock (Data)

makeLenses ''CallsOptions

parseCalls :: Parser CallsOptions
parseCalls = do
  _callsShowDecreasingArgs <-
    option
      decrArgsParser
      ( long "show-decreasing-args"
          <> short 'd'
          <> value Internal.ArgRel
          <> helpDoc (enumHelp Internal.showDecrArgsHelp)
      )
  _callsInputFile <- parseInputFile FileExtJuvix
  pure CallsOptions {..}
  where
    decrArgsParser :: ReadM Internal.ShowDecrArgs
    decrArgsParser = enumReader Proxy

instance CanonicalProjection (GlobalOptions, CallsOptions) Internal.Options where
  project (GlobalOptions {..}, CallsOptions {..}) =
    Internal.defaultOptions
      { Internal._optShowNameIds = _globalShowNameIds,
        Internal._optShowDecreasingArgs = _callsShowDecreasingArgs
      }
