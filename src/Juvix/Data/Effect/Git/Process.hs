module Juvix.Data.Effect.Git.Process where

import Data.Text qualified as T
import Juvix.Data.Effect.Git.Base
import Juvix.Data.Effect.Git.Process.Error
import Juvix.Data.Effect.Process
import Juvix.Prelude
import Polysemy.Scoped

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
        ExitSuccess -> return ""

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

initGitRepo :: (Members '[Log, Files, Process, Error GitProcessError, Reader CloneDir] r) => Text -> Sem r ()
initGitRepo url = do
  p <- ask
  unlessM (directoryExists' p) (cloneGitRepo url)

catchNotACloneError :: (Member (Error GitProcessError) r) => Sem r a -> Sem r (Either GitError a)
catchNotACloneError x = catch (Right <$> x) $ \case
  GitCmdError GitCmdErrorDetails {_gitCmdErrorDetailsExitCode = ExitFailure 128} -> return (Left NotAClone)
  e -> throw e

catchCheckoutError :: (Member (Error GitProcessError) r) => GitRef -> Sem r a -> Sem r (Either GitError a)
catchCheckoutError ref x = catch (Right <$> x) $ \case
  GitCmdError GitCmdErrorDetails {_gitCmdErrorDetailsExitCode = ExitFailure 128} -> return (Left NotAClone)
  GitCmdError GitCmdErrorDetails {_gitCmdErrorDetailsExitCode = ExitFailure 1} -> return (Left (NoSuchRef ref))
  e -> throw e

runGitProcess :: forall r a. (Members '[Log, Files, Process, Error GitProcessError] r) => Sem (Scoped CloneArgs Git ': r) a -> Sem r a
runGitProcess = interpretScopedAs allocator handler
  where
    allocator :: CloneArgs -> Sem r (Path Abs Dir)
    allocator a = runReader (a ^. cloneArgsCloneDir) (initGitRepo (a ^. cloneArgsRepoUrl)) >> return (a ^. cloneArgsCloneDir)

    handler :: Path Abs Dir -> Git m x -> Sem r x
    handler p eff = runReader p $ case eff of
      HeadRef -> catchNotACloneError gitHeadRef
      Fetch -> catchNotACloneError gitFetch
      Checkout ref -> (catchCheckoutError ref) (gitCheckout ref)
