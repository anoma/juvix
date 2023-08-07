module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Git
  ( module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Git,
    module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Git.Error,
  )
where

import Data.Text qualified as T
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Git.Error
import Juvix.Data.Effect.Process
import Juvix.Prelude

type GitUrl = Text

type GitRef = Text

data GitCloneSpec = GitCloneSpec
  { _gitSpecClonePath :: Path Abs Dir,
    _gitSpecRepoUrl :: GitUrl,
    _gitSpecRepoRef :: GitRef
  }

data Git m a where
  Clone :: GitCloneSpec -> Git m ()

makeSem ''Git

makeLenses ''GitCloneSpec

-- | Run a git command in the current working direcotory of the parent process.
runGitCmd :: Members [Process, Error GitError] r => [Text] -> Sem r Text
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
runGitCmdInDir :: Members [Process, Error GitError] r => Path Abs Dir -> [Text] -> Sem r Text
runGitCmdInDir p args = do
  checkValidGitClone p
  runGitCmdInDir' p args

-- | Run a git command within a directory
runGitCmdInDir' :: Members [Process, Error GitError] r => Path Abs Dir -> [Text] -> Sem r Text
runGitCmdInDir' p args = do
  runGitCmd (["-C", T.pack (toFilePath p)] <> args)

-- | Throws an error if the directory is not a valid git clone
checkValidGitClone :: Members [Process, Error GitError] r => Path Abs Dir -> Sem r ()
checkValidGitClone p = void (gitHeadRef p)

isValidGitClone :: Members '[Process] r => GitCloneSpec -> Sem r Bool
isValidGitClone s = isRight <$> runError @GitError (checkValidGitClone (s ^. gitSpecClonePath))

-- | Return the HEAD ref of the clone, throws NotAGitClone if the clone is invalid
gitHeadRef :: Members [Process, Error GitError] r => Path Abs Dir -> Sem r Text
gitHeadRef p = catch (T.strip <$> runGitCmdInDir' p ["rev-parse", "HEAD"]) $ \case
  (GitCmdError GitCmdErrorDetails {_gitCmdErrorDetailsExitCode = ExitFailure 128}) -> throw (NotAGitClone p)
  e -> throw e

cloneGitRepo :: Members '[Files, Process, Error GitError] r => GitUrl -> Path Abs Dir -> Sem r ()
cloneGitRepo url p = do
  -- (dirs, files) <- listDirRel p
  -- unless (null dirs && null files) (throw (NotAnEmptyDir p))
  void (runGitCmd ["clone", url, T.pack (toFilePath p)])

initGitRepo :: Members '[Files, Process, Error GitError] r => GitCloneSpec -> Sem r ()
initGitRepo s = unlessM (isValidGitClone s) (cloneGitRepo (s ^. gitSpecRepoUrl) (s ^. gitSpecClonePath))

runGit :: Members '[Files, Process, Error GitError] r => Sem (Git ': r) a -> Sem r a
runGit = interpret $ \case
  Clone g -> initGitRepo g
