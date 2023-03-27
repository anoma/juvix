module Juvix.Data.Effect.Files.Base
  ( module Juvix.Data.Effect.Files.Base,
    module Juvix.Data.Uid,
  )
where

import Juvix.Data.Uid
import Juvix.Prelude.Base
import Path

data RecursorArgs = RecursorArgs
  { _recCurDir :: Path Rel Dir,
    _recDirs :: [Path Rel Dir],
    _recFiles :: [Path Rel File]
  }

data Recurse r
  = RecurseNever
  | RecurseFilter (Bool -> Path r Dir -> Bool)

makeLenses ''RecursorArgs

data Files m a where
  EnsureDir' :: Path Abs Dir -> Files m ()
  DirectoryExists' :: Path Abs Dir -> Files m Bool
  FileExists' :: Path Abs File -> Files m Bool
  GetDirAbsPath :: Path Rel Dir -> Files m (Path Abs Dir)
  ListDirRel :: Path Abs Dir -> Files m ([Path Rel Dir], [Path Rel File])
  PathUid :: Path Abs b -> Files m Uid
  ReadFile' :: Path Abs File -> Files m Text
  ReadFileBS' :: Path Abs File -> Files m ByteString
  RemoveDirectoryRecursive' :: Path Abs Dir -> Files m ()
  WriteFile' :: Path Abs File -> Text -> Files m ()
  WriteFileBS :: Path Abs File -> ByteString -> Files m ()
  RemoveFile' :: Path Abs File -> Files m ()
  RenameFile' :: Path Abs File -> Path Abs File -> Files m ()
  CopyFile' :: Path Abs File -> Path Abs File -> Files m ()

makeSem ''Files
