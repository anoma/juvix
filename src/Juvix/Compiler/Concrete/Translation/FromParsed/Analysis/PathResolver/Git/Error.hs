module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Git.Error where

import Juvix.Data.PPOutput
import Juvix.Prelude

data GitCmdErrorDetails = GitCmdErrorDetails
  { _gitCmdErrorDetailsCmdPath :: Path Abs File,
    _gitCmdErrorDetailsArgs :: [Text],
    _gitCmdErrorDetailsExitCode :: ExitCode,
    _gitCmdErrorDetailsMessage :: Text
  }

data GitErrorCause
  = GitCmdError GitCmdErrorDetails
  | GitCmdNotFound
  | NotAGitClone (Path Abs Dir)
  | NotAnEmptyDir (Path Abs Dir)

data GitError = GitError {_gitErrorCause :: GitErrorCause, _gitErrorPackageFile :: Path Abs File}

makeLenses ''GitCmdErrorDetails
makeLenses ''GitError

instance Pretty GitCmdErrorDetails where
  pretty _ = pretty msg
    where
      msg :: Text
      msg = "error when executing the git command"

instance ToGenericError GitError where
  genericError e = return $ case e ^. gitErrorCause of
    GitCmdError ce -> prettyErr ce
    GitCmdNotFound {} -> err "git command not found"
    NotAGitClone p -> err ("path " <> show p <> " is not a git clone")
    NotAnEmptyDir p -> err ("path " <> show p <> " is not an empty dir")
    where
      prettyErr :: (Pretty a) => a -> GenericError
      prettyErr p =
        GenericError
          { _genericErrorMessage = ppOutput (pretty p),
            _genericErrorLoc = i,
            _genericErrorIntervals = [i]
          }

      err :: Text -> GenericError
      err = prettyErr
      i = getLoc e

instance HasLoc GitError where
  getLoc e = singletonInterval (mkInitialLoc (e ^. gitErrorPackageFile))
