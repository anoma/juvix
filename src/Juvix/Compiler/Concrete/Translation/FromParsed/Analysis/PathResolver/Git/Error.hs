module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Git.Error where

import Juvix.Data.PPOutput
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
  genericError e =
    return
      GenericError
        { _genericErrorMessage = ppOutput (pretty msg),
          _genericErrorLoc = i,
          _genericErrorIntervals = [i]
        }
    where
      i = getLoc e
      msg :: Text
      msg = "error when executing the git command"

instance ToGenericError GitError where
  genericError :: (Member (Reader GenericOptions) r) => GitError -> Sem r GenericError
  genericError e = case e of
    GitCmdError ce -> genericError ce
    GitCmdNotFound {} -> return (err "git command not found")
    NotAGitClone p -> return (err ("path " <> show p <> " is not a git clone"))
    NotAnEmptyDir p -> return (err ("path " <> show p <> " is not an empty dir"))
    where
      err :: Text -> GenericError
      err msg =
        GenericError
          { _genericErrorMessage = ppOutput (pretty msg),
            _genericErrorLoc = i,
            _genericErrorIntervals = [i]
          }
      i = getLoc e

instance HasLoc GitCmdErrorDetails where
  getLoc (GitCmdErrorDetails {}) = fromMaybe defaultLoc Nothing
    where
      defaultLoc :: Interval
      defaultLoc = singletonInterval (mkInitialLoc sourcePath)

      sourcePath :: Path Abs File
      sourcePath = $(mkAbsFile "/<yaml>")

instance HasLoc GitError where
  getLoc _ = fromMaybe defaultLoc Nothing
    where
      defaultLoc :: Interval
      defaultLoc = singletonInterval (mkInitialLoc sourcePath)

      sourcePath :: Path Abs File
      sourcePath = $(mkAbsFile "/<yaml>")
