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

data Git m a where
  Fetch :: (GitError -> m ()) -> Git m ()
  Checkout :: (GitError -> m ()) -> GitRef -> Git m ()
  HeadRef :: (GitError -> m GitRef) -> Git m GitRef

makeSem ''Git

type GitClone = Scoped CloneArgs Git
