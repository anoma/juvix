module Juvix.Data.Effect.Git where

import Data.Text qualified as T
import Juvix.Data.Effect.Process
import Juvix.Prelude

newtype GitRef = GitRef {_gitRef :: Text}

newtype GitUrl = GitUrl {_gitUrl :: Text}

-- | GitOps operations that can be performed on a clone
data GitOps m a where
  Checkout :: GitRef -> GitOps m ()
  HeadRef :: GitOps m Text

data GitCloner m a where
  Clone :: GitUrl -> Path Abs Dir -> GitCloner m ()

makeSem ''GitOps
makeSem ''GitCloner

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

runGitCloner :: Members [Files, Process, Error GitClonerError] r => Sem (GitCloner ': r) a -> Sem r a
runGitCloner = interpret $ \case
  Clone (GitUrl url) p -> do
    (dirs, files) <- listDirRel p
    unless (null dirs && null files) (throw (NotAnEmptyDir p))
    mapError GitClonerCmdError (void (runGitCmd ["clone", url, T.pack (toFilePath p)]))

-- | Perform git operations within a clone directory
withGitClone :: forall r a. Members [Process, Error GitOpsError] r => Path Abs Dir -> Sem (GitOps ': r) a -> Sem r a
withGitClone p m = runReader p (withGitClone' m)

withGitClone' :: forall r a. Members [Process, Error GitOpsError] r => Sem (GitOps ': r) a -> Sem (Reader (Path Abs Dir) ': r) a
withGitClone' = reinterpret $ \case
  Checkout (GitRef ref) -> void (runGitCmdInDir ["checkout", ref])
  HeadRef -> gitHeadRef

-- | Run a git command within a directory, throws an error if the directory is not a valid clone
runGitCmdInDir :: Members [Process, Error GitOpsError, Reader (Path Abs Dir)] r => [Text] -> Sem r Text
runGitCmdInDir args = do
  checkValidGitClone
  runGitCmdInDir' args

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
runGitCmdInDir' :: Members [Process, Error GitOpsError, Reader (Path Abs Dir)] r => [Text] -> Sem r Text
runGitCmdInDir' args = do
  p :: Path Abs Dir <- ask
  mapError GitOpsCmdError (runGitCmd (["-C", T.pack (toFilePath p)] <> args))

-- | Throws an error if the directory is not a valid git clone
checkValidGitClone :: Members [Process, Error GitOpsError, Reader (Path Abs Dir)] r => Sem r ()
checkValidGitClone = void (gitHeadRef)

-- | Return the HEAD ref of the clone, throws NotAGitClone if the clone is invalid
gitHeadRef :: Members [Process, Error GitOpsError, Reader (Path Abs Dir)] r => Sem r Text
gitHeadRef = catch (T.strip <$> runGitCmdInDir' ["rev-parse", "HEAD"]) $ \case
  GitOpsCmdError (GitCmdError GitCmdErrorDetails {_gitCmdErrorDetailsExitCode = ExitFailure 128}) -> ask >>= throw . NotAGitClone
  e -> throw e
