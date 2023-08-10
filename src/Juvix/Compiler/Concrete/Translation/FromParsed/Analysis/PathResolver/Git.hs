module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Git
  ( module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Git,
    module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Git.Error,
  )
where

import Data.Text qualified as T
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Git.Error
import Juvix.Data.Effect.Process
import Juvix.Prelude
import Juvix.Compiler.Pipeline.Package
import Juvix.Extra.Paths

data Git m a where
  Clone :: Path Abs Dir -> GitDependency -> Git m (Path Abs Dir)

makeSem ''Git

cloneDir :: Path Abs Dir -> GitDependency -> Path Abs Dir
cloneDir r d = r <//> relDependenciesDir <//> relDir (T.unpack (d ^. gitDependencyName))

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
runGitCmdInDir :: Members [Process, Error GitError, Reader (Path Abs Dir)] r => [Text] -> Sem r Text
runGitCmdInDir args = do
  checkValidGitClone
  runGitCmdInDir' args

-- | Run a git command within a directory
runGitCmdInDir' :: Members [Process, Error GitError, Reader (Path Abs Dir)] r => [Text] -> Sem r Text
runGitCmdInDir' args = do
  p :: Path Abs Dir <- ask
  runGitCmd (["-C", T.pack (toFilePath p)] <> args)

-- | Throws an error if the directory is not a valid git clone
checkValidGitClone :: Members [Process, Error GitError, Reader (Path Abs Dir)] r => Sem r ()
checkValidGitClone = void gitHeadRef

isValidGitClone :: Members '[Process, Reader (Path Abs Dir)] r =>  Sem r Bool
isValidGitClone = isRight <$> runError @GitError checkValidGitClone

-- | Return the HEAD ref of the clone, throws NotAGitClone if the clone is invalid
gitHeadRef :: Members [Process, Error GitError, Reader (Path Abs Dir)] r =>  Sem r Text
gitHeadRef = catch (T.strip <$> runGitCmdInDir' ["rev-parse", "HEAD"]) $ \case
  (GitCmdError GitCmdErrorDetails {_gitCmdErrorDetailsExitCode = ExitFailure 128}) -> do
    p <- ask
    throw (NotAGitClone p)
  e -> throw e

cloneGitRepo :: Members '[Files, Process, Error GitError, Reader (Path Abs Dir)] r => Text -> Sem r ()
cloneGitRepo url = do
  -- (dirs, files) <- listDirRel p
  -- unless (null dirs && null files) (throw (NotAnEmptyDir p))
  p :: Path Abs Dir <- ask
  void (runGitCmd ["clone", url, T.pack (toFilePath p)])

initGitRepo :: Members '[Files, Process, Error GitError, Reader (Path Abs Dir)] r => GitDependency -> Sem r ()
initGitRepo d = unlessM isValidGitClone (cloneGitRepo (d ^. gitDependencyUrl))

runGit :: Members '[Files, Process, Error GitError] r => Sem (Git ': r) a -> Sem r a
runGit = interpret $ \case
  Clone r d -> let outputDir = cloneDir r d in runReader (cloneDir r d) (initGitRepo d) >> return outputDir
