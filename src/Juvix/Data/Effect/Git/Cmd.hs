module Juvix.Data.Effect.Git.Cmd where

import Data.Text qualified as T
import Juvix.Data.Effect.Git.Base
import Juvix.Data.Effect.Process
import Juvix.Prelude
import Polysemy.Scoped

newtype GitUrl = GitUrl {_gitUrl :: Text}

data GitCmdErrorDetails = GitCmdErrorDetails
  { _gitCmdErrorDetailsCmdPath :: Path Abs File,
    _gitCmdErrorDetailsArgs :: [Text],
    _gitCmdErrorDetailsExitCode :: ExitCode,
    _gitCmdErrorDetailsMessage :: Text
  }

makeLenses ''GitCmdErrorDetails

-- | Errors arising from git command invocations
data GitCmdError
  = GitCmdNotFound
  | GitCmdError GitCmdErrorDetails

-- | Errors arising from git operations within a clone
data GitOpsError
  = GitOpsCmdError GitCmdError
  | NotAGitClone (Path Abs Dir)

-- | Errors arising when making a git clone
data GitClonerError
  = GitClonerCmdError GitCmdError
  | NotAnEmptyDir (Path Abs Dir)

runGitFsClone :: Members '[Process, Error GitOpsError] r => Path Abs Dir -> Sem (Git ': r) a -> Sem r a
runGitFsClone p = interpret $ \case
  Checkout (GitRef ref) -> void (runGitCmdInDir p ["checkout", ref])
  HeadRef -> gitHeadRef p

-- | Run a git command within a directory, throws an error if the directory is not a valid clone
runGitCmdInDir :: Members [Process, Error GitOpsError] r => Path Abs Dir -> [Text] -> Sem r Text
runGitCmdInDir p args = do
  checkValidGitClone p
  runGitCmdInDir' p args

-- | Run a git command in the current working direcotory of the parent process.
runGitCmd :: Members [Process, Error GitCmdError] r => [Text] -> Sem r Text
runGitCmd args = do
  mcmd <- findExecutable' $(mkRelFile "git")
  case mcmd of
    Nothing -> throw GitCmdNotFound
    Just cmd -> do
      res <- readProcess' (ProcessCall {_processCallPath = cmd, _processCallArgs = args})
      case res ^. processResultExitCode of
        ExitFailure {} ->
          throw
            ( GitCmdError
                ( GitCmdErrorDetails
                    { _gitCmdErrorDetailsCmdPath = cmd,
                      _gitCmdErrorDetailsArgs = args,
                      _gitCmdErrorDetailsExitCode = res ^. processResultExitCode,
                      _gitCmdErrorDetailsMessage = res ^. processResultStderr
                    }
                )
            )
        ExitSuccess -> return ""

-- | Run a git command within a directory
runGitCmdInDir' :: Members [Process, Error GitOpsError] r => Path Abs Dir -> [Text] -> Sem r Text
runGitCmdInDir' p args = do
  mapError GitOpsCmdError (runGitCmd (["-C", T.pack (toFilePath p)] <> args))

-- | Throws an error if the directory is not a valid git clone
checkValidGitClone :: Members [Process, Error GitOpsError] r => Path Abs Dir -> Sem r ()
checkValidGitClone p = void (gitHeadRef p)

isValidGitClone :: Members '[Process] r => Path Abs Dir -> Sem r Bool
isValidGitClone p = isRight <$> runError @GitOpsError (checkValidGitClone p)

-- | Return the HEAD ref of the clone, throws NotAGitClone if the clone is invalid
gitHeadRef :: Members [Process, Error GitOpsError] r => Path Abs Dir -> Sem r Text
gitHeadRef p = catch (T.strip <$> runGitCmdInDir' p ["rev-parse", "HEAD"]) $ \case
  GitOpsCmdError (GitCmdError GitCmdErrorDetails {_gitCmdErrorDetailsExitCode = ExitFailure 128}) -> throw (NotAGitClone p)
  e -> throw e

cloneGitRepo :: Members '[Files, Process, Error GitClonerError] r => GitUrl -> Path Abs Dir -> Sem r ()
cloneGitRepo (GitUrl url) p = do
  (dirs, files) <- listDirRel p
  unless (null dirs && null files) (throw (NotAnEmptyDir p))
  mapError GitClonerCmdError (void (runGitCmd ["clone", url, T.pack (toFilePath p)]))

initGitRepo :: Members '[Files, Process, Error GitClonerError] r => GitUrl -> Path Abs Dir -> Sem r (Path Abs Dir)
initGitRepo gitUrl p = unlessM (isValidGitClone p) (cloneGitRepo gitUrl p) >> return p
