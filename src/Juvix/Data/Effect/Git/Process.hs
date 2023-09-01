module Juvix.Data.Effect.Git.Process where

import Data.Text qualified as T
import Juvix.Data.Effect.Git.Base
import Juvix.Data.Effect.Git.Process.Error
import Juvix.Data.Effect.Process
import Juvix.Prelude
import Polysemy.Opaque

-- | Run a git command in the current working direcotory of the parent process.
runGitCmd :: (Members '[Process, Error GitProcessError] r) => [Text] -> Sem r Text
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
        ExitSuccess -> return (res ^. processResultStdout)

-- | Run a git command within a directory, throws an error if the directory is not a valid clone
runGitCmdInDir :: (Members '[Process, Error GitProcessError, Reader CloneDir] r) => [Text] -> Sem r Text
runGitCmdInDir args = do
  checkValidGitClone
  runGitCmdInDir' args

-- | Run a git command within a directory
runGitCmdInDir' :: (Members '[Process, Error GitProcessError, Reader CloneDir] r) => [Text] -> Sem r Text
runGitCmdInDir' args = do
  p :: Path Abs Dir <- ask
  runGitCmd (["--git-dir", ".git", "-C", T.pack (toFilePath p)] <> args)

-- | Throws an error if the directory is not a valid git clone
checkValidGitClone :: (Members '[Process, Error GitProcessError, Reader CloneDir] r) => Sem r ()
checkValidGitClone = void gitHeadRef

isValidGitClone :: (Members '[Process, Reader CloneDir] r) => Sem r Bool
isValidGitClone = isRight <$> runError @GitProcessError checkValidGitClone

-- | Return the HEAD ref of the clone
gitHeadRef :: (Members '[Process, Error GitProcessError, Reader CloneDir] r) => Sem r Text
gitHeadRef = T.strip <$> runGitCmdInDir' ["rev-parse", "HEAD"]

-- | Checkout the clone at a particular ref
gitCheckout :: (Members '[Process, Error GitProcessError, Reader CloneDir] r) => Text -> Sem r ()
gitCheckout ref = void (runGitCmdInDir ["checkout", ref])

-- | Fetch in the clone
gitFetch :: (Members '[Process, Error GitProcessError, Reader CloneDir] r) => Sem r ()
gitFetch = void (runGitCmdInDir ["fetch"])

cloneGitRepo :: (Members '[Log, Files, Process, Error GitProcessError, Reader CloneDir] r) => Text -> Sem r ()
cloneGitRepo url = do
  p :: Path Abs Dir <- ask
  log ("cloning " <> url <> " to " <> pack (toFilePath p))
  void (runGitCmd ["clone", url, T.pack (toFilePath p)])

initGitRepo :: (Members '[Log, Files, Process, Error GitProcessError, Reader CloneDir] r) => Text -> Sem r (Path Abs Dir)
initGitRepo url = do
  p <- ask
  unlessM (directoryExists' p) (cloneGitRepo url)
  return p

handleNotACloneError :: (Member (Error GitProcessError) r, Monad m) => (GitError -> m x) -> Tactical e m r x -> Tactical e m r x
handleNotACloneError errorHandler eff = catch @GitProcessError eff $ \case
  GitCmdError GitCmdErrorDetails {_gitCmdErrorDetailsExitCode = ExitFailure 128} -> runTSimple (return NotAClone) >>= bindTSimple errorHandler
  e -> throw e

handleNoSuchRefError :: (Member (Error GitProcessError) r, Monad m) => (GitError -> m x) -> GitRef -> Tactical e m r x -> Tactical e m r x
handleNoSuchRefError errorHandler ref eff = catch @GitProcessError eff $ \case
  GitCmdError GitCmdErrorDetails {_gitCmdErrorDetailsExitCode = ExitFailure 1} -> runTSimple (return (NoSuchRef ref)) >>= bindTSimple errorHandler
  e -> throw e

handleCheckoutError :: (Member (Error GitProcessError) r, Monad m) => (GitError -> m x) -> GitRef -> Tactical e m r x -> Tactical e m r x
handleCheckoutError errorHandler ref eff = handleNoSuchRefError errorHandler ref (handleNotACloneError errorHandler eff)

runGitProcess ::
  forall r a.
  (Members '[Log, Files, Process, Error GitProcessError] r) =>
  Sem (Scoped CloneArgs Git ': r) a ->
  Sem r a
runGitProcess = interpretScopedH allocator handler
  where
    allocator :: forall q x. CloneArgs -> (Path Abs Dir -> Sem (Opaque q ': r) x) -> Sem (Opaque q ': r) x
    allocator a use' = use' =<< runReader (a ^. cloneArgsCloneDir) (initGitRepo (a ^. cloneArgsRepoUrl))

    handler :: forall q r0 x. Path Abs Dir -> Git (Sem r0) x -> Tactical Git (Sem r0) (Opaque q ': r) x
    handler p eff = case eff of
      HeadRef errorHandler -> handleNotACloneError errorHandler (runReader p gitHeadRef >>= pureT)
      Fetch errorHandler -> handleNotACloneError errorHandler (runReader p gitFetch >>= pureT)
      Checkout errorHandler ref -> handleCheckoutError errorHandler ref (runReader p (gitCheckout ref) >>= pureT)
