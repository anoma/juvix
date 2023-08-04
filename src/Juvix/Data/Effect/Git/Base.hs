module Juvix.Data.Effect.Git.Base where

import Juvix.Prelude

newtype GitRef = GitRef {_gitRef :: Text}

data Git m a where
  Checkout :: GitRef -> Git m ()
  HeadRef :: Git m Text

makeSem ''Git
