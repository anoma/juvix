module Juvix.Data.Effect.Git.Process where

import Data.Text qualified as T
import Juvix.Data.Effect.Git.Base
import Juvix.Data.Effect.Git.Process.Error
import Juvix.Data.Effect.Process
import Juvix.Data.Effect.TaggedLock
import Juvix.Prelude
import Polysemy.Opaque

newtype CloneEnv = CloneEnv
  {_cloneEnvDir :: Path Abs Dir}

makeLenses ''CloneEnv

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
runGitCmdInDir :: (Members '[Process, Error GitProcessError, Reader CloneEnv] r) => [Text] -> Sem r Text
runGitCmdInDir args = do
  checkValidGitClone
  runGitCmdInDir' args

-- | Run a git command within a directory
runGitCmdInDir' :: (Members '[Process, Error GitProcessError, Reader CloneEnv] r) => [Text] -> Sem r Text
runGitCmdInDir' args = do
  p <- asks (^. cloneEnvDir)
  runGitCmd (["--git-dir", ".git", "-C", T.pack (toFilePath p)] <> args)

-- | Throws an error if the directory is not a valid git clone
checkValidGitClone :: (Members '[Process, Error GitProcessError, Reader CloneEnv] r) => Sem r ()
checkValidGitClone = void gitHeadRef

isValidGitClone :: (Members '[Process, Reader CloneEnv] r) => Sem r Bool
isValidGitClone = isRight <$> runError @GitProcessError checkValidGitClone

-- | Return the normal form of the passed git reference
gitNormalizeRef :: forall r. (Members '[Process, Error GitProcessError, Reader CloneEnv] r) => Text -> Sem r Text
gitNormalizeRef ref = T.strip <$> runGitCmdInDir' ["rev-parse", "--verify", ref <> "^{commit}"]

-- | Return the HEAD ref of the clone
gitHeadRef :: (Members '[Process, Error GitProcessError, Reader CloneEnv] r) => Sem r Text
gitHeadRef = gitNormalizeRef "HEAD"

-- | Checkout the clone at a particular ref
gitCheckout :: (Members '[TaggedLock, Process, Error GitProcessError, Reader CloneEnv] r) => Text -> Sem r ()
gitCheckout ref = withTaggedLockDir' (void (runGitCmdInDir ["checkout", ref]))

-- | Fetch in the clone
gitFetch :: (Members '[TaggedLock, Process, Error GitProcessError, Reader CloneEnv, Internet] r) => Sem r ()
gitFetch = whenHasInternet gitFetchOnline

gitFetchOnline :: (Members '[TaggedLock, Reader CloneEnv, Error GitProcessError, Process, Online] r) => Sem r ()
gitFetchOnline = withTaggedLockDir' (void (runGitCmdInDir ["fetch"]))

gitCloneOnline :: (Members '[Log, Error GitProcessError, Process, Online, Reader CloneEnv] r) => Text -> Sem r ()
gitCloneOnline url = do
  p <- asks (^. cloneEnvDir)
  log ("Cloning " <> url <> " to " <> pack (toFilePath p))
  void (runGitCmd ["clone", url, T.pack (toFilePath p)])

cloneGitRepo :: (Members '[Log, Files, Process, Error GitProcessError, Reader CloneEnv, Internet] r) => Text -> Sem r ()
cloneGitRepo = whenHasInternet . gitCloneOnline

initGitRepo :: (Members '[TaggedLock, Log, Files, Process, Error GitProcessError, Reader CloneEnv, Internet] r) => Text -> Sem r (Path Abs Dir)
initGitRepo url = do
  p <- asks (^. cloneEnvDir)
  withTaggedLockDir' (unlessM (directoryExists' p) (cloneGitRepo url))
  return p

handleNotACloneError :: (Member (Error GitProcessError) r, Monad m) => (GitError -> m x) -> Tactical e m r x -> Tactical e m r x
handleNotACloneError errorHandler eff = catch @GitProcessError eff $ \case
  GitCmdError GitCmdErrorDetails {_gitCmdErrorDetailsExitCode = ExitFailure 128} -> runTSimple (return NotAClone) >>= bindTSimple errorHandler
  e -> throw e

handleNormalizeRefError :: (Member (Error GitProcessError) r, Monad m) => (GitError -> m x) -> GitRef -> Tactical e m r x -> Tactical e m r x
handleNormalizeRefError errorHandler ref eff = catch @GitProcessError eff $ \case
  GitCmdError GitCmdErrorDetails {_gitCmdErrorDetailsExitCode = ExitFailure 128} -> runTSimple (return (NoSuchRef ref)) >>= bindTSimple errorHandler
  e -> throw e

withTaggedLockDir' :: (Members '[TaggedLock, Reader CloneEnv] r) => Sem r a -> Sem r a
withTaggedLockDir' ma = do
  p <- asks (^. cloneEnvDir)
  withTaggedLockDir p ma

runGitProcess ::
  forall r a.
  (Members '[TaggedLock, Log, Files, Process, Error GitProcessError, Internet] r) =>
  Sem (Scoped CloneArgs Git ': r) a ->
  Sem r a
runGitProcess = interpretScopedH allocator handler
  where
    allocator :: forall q x. CloneArgs -> (Path Abs Dir -> Sem (Opaque q ': r) x) -> Sem (Opaque q ': r) x
    allocator a use' = do
      let env = CloneEnv {_cloneEnvDir = a ^. cloneArgsCloneDir}
      use' =<< runReader env (initGitRepo (a ^. cloneArgsRepoUrl))

    handler :: forall q r0 x. Path Abs Dir -> Git (Sem r0) x -> Tactical Git (Sem r0) (Opaque q ': r) x
    handler p eff = case eff of
      Fetch errorHandler -> handleNotACloneError errorHandler (runReader env gitFetch >>= pureT)
      Checkout errorHandler ref -> do
        void (handleNormalizeRefError errorHandler ref (runReader env (void (gitNormalizeRef ref)) >>= pureT))
        handleNotACloneError errorHandler (runReader env (gitCheckout ref) >>= pureT)
      NormalizeRef errorHandler ref -> handleNormalizeRefError errorHandler ref (runReader env (gitNormalizeRef ref) >>= pureT)
      where
        env :: CloneEnv
        env = CloneEnv {_cloneEnvDir = p}
