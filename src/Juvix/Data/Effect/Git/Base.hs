module Juvix.Data.Effect.Git.Base where

import Juvix.Prelude
import Polysemy.Scoped

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
  Fetch :: Git m (Either GitError ())
  Checkout :: GitRef -> Git m (Either GitError ())
  HeadRef :: Git m (Either GitError GitRef)

makeSem ''Git

type GitClone = Scoped CloneArgs Git
