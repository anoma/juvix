module Juvix.Data.Effect.Git.Process where

import Data.Text qualified as T
import Juvix.Data.Effect.Git.Base
import Juvix.Data.Effect.Git.Process.Error
import Juvix.Data.Effect.Process
import Juvix.Data.Effect.TaggedLock
import Juvix.Prelude

newtype CloneEnv = CloneEnv
  {_cloneEnvDir :: Path Abs Dir}

makeLenses ''CloneEnv

-- | Run a git command in the current working directory of the parent process.
runGitCmd :: (Members '[ProcessE, Error GitProcessError] r) => [Text] -> Sem r Text
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
runGitCmdInDir :: (Members '[ProcessE, Error GitProcessError, Reader CloneEnv] r) => [Text] -> Sem r Text
runGitCmdInDir args = do
  checkValidGitClone
  runGitCmdInDir' args

-- | Run a git command within a directory
runGitCmdInDir' :: (Members '[ProcessE, Error GitProcessError, Reader CloneEnv] r) => [Text] -> Sem r Text
runGitCmdInDir' args = do
  p <- asks (^. cloneEnvDir)
  runGitCmd (["--git-dir", ".git", "-C", T.pack (toFilePath p)] <> args)

-- | Throws an error if the directory is not a valid git clone
checkValidGitClone :: (Members '[ProcessE, Error GitProcessError, Reader CloneEnv] r) => Sem r ()
checkValidGitClone = void gitHeadRef

isValidGitClone :: (Members '[ProcessE, Reader CloneEnv] r) => Sem r Bool
isValidGitClone = isRight <$> runError @GitProcessError checkValidGitClone

-- | Return the normal form of the passed git reference
gitNormalizeRef :: forall r. (Members '[ProcessE, Error GitProcessError, Reader CloneEnv] r) => Text -> Sem r Text
gitNormalizeRef ref = T.strip <$> runGitCmdInDir' ["rev-parse", "--verify", ref <> "^{commit}"]

-- | Return the HEAD ref of the clone
gitHeadRef :: (Members '[ProcessE, Error GitProcessError, Reader CloneEnv] r) => Sem r Text
gitHeadRef = gitNormalizeRef "HEAD"

-- | Checkout the clone at a particular ref
gitCheckout :: (Members '[TaggedLock, ProcessE, Error GitProcessError, Reader CloneEnv] r) => Text -> Sem r ()
gitCheckout ref = withTaggedLockDir' (void (runGitCmdInDir ["checkout", ref]))

-- | Fetch in the clone
gitFetch :: (Members '[TaggedLock, ProcessE, Error GitProcessError, Reader CloneEnv, Internet] r) => Sem r ()
gitFetch = whenHasInternet gitFetchOnline

gitFetchOnline :: (Members '[TaggedLock, Reader CloneEnv, Error GitProcessError, ProcessE, Online] r) => Sem r ()
gitFetchOnline = withTaggedLockDir' (void (runGitCmdInDir ["fetch"]))

gitCloneOnline :: (Members '[Log, Error GitProcessError, ProcessE, Online, Reader CloneEnv] r) => Text -> Sem r ()
gitCloneOnline url = do
  p <- asks (^. cloneEnvDir)
  log ("Cloning " <> url <> " to " <> pack (toFilePath p))
  void (runGitCmd ["clone", url, T.pack (toFilePath p)])

cloneGitRepo :: (Members '[Log, Files, ProcessE, Error GitProcessError, Reader CloneEnv, Internet] r) => Text -> Sem r ()
cloneGitRepo = whenHasInternet . gitCloneOnline

initGitRepo :: (Members '[TaggedLock, Log, Files, ProcessE, Error GitProcessError, Reader CloneEnv, Internet] r) => Text -> Sem r (Path Abs Dir)
initGitRepo url = do
  p <- asks (^. cloneEnvDir)
  withTaggedLockDir' (unlessM (directoryExists' p) (cloneGitRepo url))
  return p

handleNotACloneError :: (Member (Error GitProcessError) r) => LocalEnv localEs r -> (GitError -> Sem localEs x) -> Sem r x -> Sem r x
handleNotACloneError localEnv errorHandler eff = catch @GitProcessError eff $ \case
  GitCmdError
    GitCmdErrorDetails
      { _gitCmdErrorDetailsExitCode = ExitFailure 128
      } ->
      runTSimpleEff localEnv (errorHandler NotAClone)
  e -> throw e

handleNormalizeRefError :: (Member (Error GitProcessError) r) => LocalEnv localEs r -> (GitError -> Sem localEs x) -> GitRef -> Sem r x -> Sem r x
handleNormalizeRefError localEnv errorHandler ref eff = catch @GitProcessError eff $ \case
  GitCmdError
    GitCmdErrorDetails
      { _gitCmdErrorDetailsExitCode = ExitFailure 128
      } ->
      runTSimpleEff localEnv (errorHandler (NoSuchRef ref))
  e -> throw e

withTaggedLockDir' :: (Members '[TaggedLock, Reader CloneEnv] r) => Sem r a -> Sem r a
withTaggedLockDir' ma = do
  p <- asks (^. cloneEnvDir)
  withTaggedLockDir p ma

runGitProcess ::
  forall r a.
  (Members '[TaggedLock, Log, Files, ProcessE, Error GitProcessError, Internet] r) =>
  Sem (GitClone ': r) a ->
  Sem r a
runGitProcess = runProvider_ helper
  where
    helper :: forall x. CloneArgs -> Sem (Git ': r) x -> Sem r x
    helper cloneArgs m = do
      let env0 = CloneEnv {_cloneEnvDir = cloneArgs ^. cloneArgsCloneDir}
      clonePath <- runReader env0 (initGitRepo (cloneArgs ^. cloneArgsRepoUrl))
      let env :: CloneEnv
          env = CloneEnv {_cloneEnvDir = clonePath}
      (`interpretH` m) $ \localEnv -> \case
        Fetch errorHandler -> handleNotACloneError localEnv errorHandler (runReader env gitFetch)
        NormalizeRef errorHandler ref -> handleNormalizeRefError localEnv errorHandler ref (runReader env (gitNormalizeRef ref))
        Checkout errorHandler ref -> do
          void (handleNormalizeRefError localEnv errorHandler ref (runReader env (void (gitNormalizeRef ref))))
          handleNotACloneError localEnv errorHandler (runReader env (gitCheckout ref))
