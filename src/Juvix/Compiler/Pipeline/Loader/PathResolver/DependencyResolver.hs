module Juvix.Compiler.Pipeline.Loader.PathResolver.DependencyResolver where

import Data.Text qualified as T
import Juvix.Compiler.Pipeline.Loader.PathResolver.Data
import Juvix.Compiler.Pipeline.Loader.PathResolver.Error
import Juvix.Compiler.Pipeline.Package.Dependency
import Juvix.Data.Effect.Git
import Juvix.Data.SHA256 qualified as SHA256
import Juvix.Extra.Paths.Base
import Juvix.Prelude

data DependencyResolver' :: Effect where
  ResolveDependency' :: PackageDependencyInfo -> DependencyResolver' m ResolvedDependency

makeSem ''DependencyResolver'

type DependencyResolver = Provider_ DependencyResolver' ResolverEnv

runDependencyResolver ::
  forall r a.
  (Members '[Files, Error DependencyError, GitClone] r) =>
  Sem (DependencyResolver ': r) a ->
  Sem r a
runDependencyResolver = runProvider_ helper
  where
    helper :: forall x. ResolverEnv -> Sem (DependencyResolver' ': r) x -> Sem r x
    helper env m = do
      (`interpret` m) $ \case
        ResolveDependency' i -> case i ^. packageDepdendencyInfoDependency of
          DependencyPath p -> do
            let r = env ^. envRoot
            p' <- canonicalDir r (p ^. pathDependencyPath)
            return
              ResolvedDependency
                { _resolvedDependencyPath = p',
                  _resolvedDependencyDependency = i ^. packageDepdendencyInfoDependency
                }
          DependencyGit g -> do
            let r = rootBuildDir (env ^. envProjectRoot)
            gitCacheDir <- globalGitCache
            let cloneRelDir :: Path Rel Dir
                cloneRelDir = mkSafeDir (g ^. gitDependencyUrl)
                cloneDir = gitCacheDir <//> cloneRelDir
                cloneArgs =
                  CloneArgs
                    { _cloneArgsCloneDir = cloneDir,
                      _cloneArgsRepoUrl = g ^. gitDependencyUrl
                    }
            provideWith_ cloneArgs $ do
              fetchOnNoSuchRefAndRetry (errorHandler cloneDir) (`checkout` (g ^. gitDependencyRef))
              resolvedRef <- headRef (errorHandler cloneDir)
              let destDir =
                    r
                      <//> relDependenciesDir
                      <//> mkSafeDir (g ^. gitDependencyUrl <> resolvedRef)
              unlessM (directoryExists' destDir) (replaceDirectory cloneDir destDir)
              return
                ResolvedDependency
                  { _resolvedDependencyPath = destDir,
                    _resolvedDependencyDependency =
                      DependencyGit (set gitDependencyRef resolvedRef g)
                  }
            where
              errorHandler :: forall b. Path Abs Dir -> GitError -> Sem (Git ': r) b
              errorHandler p c =
                throw
                  DependencyError
                    { _dependencyErrorCause =
                        GitDependencyError
                          DependencyErrorGit
                            { _dependencyErrorGitCloneDir = p,
                              _dependencyErrorGitError = c
                            },
                      _dependencyErrorPackageFile = i ^. packageDependencyInfoPackageFile
                    }

              mkSafeDir :: Text -> Path Rel Dir
              mkSafeDir = relDir . T.unpack . SHA256.digestText

resolveDependency ::
  (Members '[Reader ResolverEnv, DependencyResolver] r) =>
  PackageDependencyInfo ->
  Sem r ResolvedDependency
resolveDependency i = do
  env <- ask @ResolverEnv
  provideWith_ @DependencyResolver' env (resolveDependency' i)
