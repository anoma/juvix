module Juvix.Data.Effect.Git.Process.Error where

import Juvix.Data.PPOutput
import Juvix.Prelude

data GitCmdErrorDetails = GitCmdErrorDetails
  { _gitCmdErrorDetailsCmdPath :: Path Abs File,
    _gitCmdErrorDetailsArgs :: [Text],
    _gitCmdErrorDetailsExitCode :: ExitCode,
    _gitCmdErrorDetailsMessage :: Text
  }

data GitProcessError
  = GitCmdError GitCmdErrorDetails
  | GitCmdNotFound

makeLenses ''GitCmdErrorDetails
makeLenses ''GitProcessError

instance Pretty GitCmdErrorDetails where
  pretty d = pretty msg
    where
      msg :: Text
      msg = "Error ocurred when executing the git command with arguments: " <> show (d ^. gitCmdErrorDetailsArgs)

instance Pretty GitProcessError where
  pretty = \case
    GitCmdError ce -> pretty ce
    GitCmdNotFound {} -> "Error occurred when attempting to execute the git command: 'not found'."

instance ToGenericError GitProcessError where
  genericError e =
    return
      ( GenericError
          { _genericErrorMessage = ppOutput (pretty e),
            _genericErrorLoc = i,
            _genericErrorIntervals = [i]
          }
      )
    where
      i = singletonInterval (mkInitialLoc $(mkAbsFile "/<git>"))
