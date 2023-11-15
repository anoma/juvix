module Juvix.Data.Effect.TaggedLock
  ( module Juvix.Data.Effect.TaggedLock,
    module Juvix.Data.Effect.TaggedLock.Base,
    module Juvix.Data.Effect.TaggedLock.Permissive,
    module Juvix.Data.Effect.TaggedLock.IO,
  )
where

import Juvix.Data.Effect.TaggedLock.Base
import Juvix.Data.Effect.TaggedLock.IO
import Juvix.Data.Effect.TaggedLock.Permissive
import Juvix.Prelude

-- | A variant of `withTaggedLock` that accepts an absolute directory as a tag.
--
-- The absolute path does not need to exist in the filesystem.
--
-- Example:
--
-- @
--      runFinal
--        . resourceToIOFinal
--        . embedToFinal @IO
--        . runFilesIO
--        . runTaggedLockIO
--        $ withTaggedLockDir $(mkAbsDir "/a/b/c") (embed (putStrLn "Hello" >> hFlush stdout))
-- @
withTaggedLockDir :: (Member TaggedLock r) => Path Abs Dir -> Sem r a -> Sem r a
withTaggedLockDir d = do
  let lockFile = $(mkRelFile ".lock")
      p = maybe lockFile (<//> lockFile) (dropDrive d)
  withTaggedLock p

data LockMode = LockModePermissive | LockModeExclusive

runTaggedLock :: (Members '[Files, Resource, Embed IO] r) => LockMode -> Sem (TaggedLock ': r) a -> Sem r a
runTaggedLock = \case
  LockModePermissive -> runTaggedLockPermissive
  LockModeExclusive -> runTaggedLockIO
