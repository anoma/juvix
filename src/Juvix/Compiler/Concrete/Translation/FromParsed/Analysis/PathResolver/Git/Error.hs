module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Git.Error where

import Juvix.Prelude

data GitCmdErrorDetails = GitCmdErrorDetails
  { _gitCmdErrorDetailsCmdPath :: Path Abs File,
    _gitCmdErrorDetailsArgs :: [Text],
    _gitCmdErrorDetailsExitCode :: ExitCode,
    _gitCmdErrorDetailsMessage :: Text
  }

data GitError
  = GitCmdError GitCmdErrorDetails
  | GitCmdNotFound
  | NotAGitClone (Path Abs Dir)
  | NotAnEmptyDir (Path Abs Dir)

makeLenses ''GitCmdErrorDetails

instance ToGenericError GitCmdErrorDetails where
  genericError e = undefined

instance ToGenericError GitError where
  genericError e = undefined
