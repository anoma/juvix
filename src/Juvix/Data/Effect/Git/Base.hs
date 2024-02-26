module Juvix.Data.Effect.Git.Base where

import Juvix.Prelude

type GitRef = Text

type CloneDir = Path Abs Dir

type RepoUrl = Text

data CloneArgs = CloneArgs
  { _cloneArgsCloneDir :: CloneDir,
    _cloneArgsRepoUrl :: RepoUrl
  }

makeLenses ''CloneArgs

data GitError
  = NotAClone
  | NoSuchRef GitRef

data Git :: Effect where
  Fetch :: (GitError -> m ()) -> Git m ()
  Checkout :: (GitError -> m ()) -> GitRef -> Git m ()
  NormalizeRef :: (GitError -> m GitRef) -> GitRef -> Git m GitRef

makeSem ''Git

type GitClone = Scoped CloneArgs Git

headRef :: (Member Git r) => (GitError -> Sem r GitRef) -> Sem r GitRef
headRef h = normalizeRef h "HEAD"

-- | If an action fails because a ref does not exist in the clone, first do a fetch and then retry.
fetchOnNoSuchRefAndRetry :: forall r a. (Member Git r) => (GitError -> Sem r a) -> ((GitError -> Sem r a) -> Sem r a) -> Sem r a
fetchOnNoSuchRefAndRetry handler action = action retryHandler
  where
    retryHandler :: GitError -> Sem r a
    retryHandler = \case
      NoSuchRef _ -> fetch (void . handler) >> action handler
      e -> handler e
