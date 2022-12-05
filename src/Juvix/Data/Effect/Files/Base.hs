module Juvix.Data.Effect.Files.Base
  ( module Juvix.Data.Effect.Files.Base,
    module Juvix.Data.Effect.Files.Error,
    module Juvix.Data.Uid,
  )
where

import Juvix.Data.Effect.Files.Error
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
  | RecurseFilter (Path r Dir -> Bool)

makeLenses ''RecursorArgs

data Files m a where
  CanonicalizePath' :: FilePath -> Files m FilePath
  CreateDirectoryIfMissing' :: Path b Dir -> Files m ()
  DirectoryExists' :: Path a Dir -> Files m Bool
  EqualPaths' :: FilePath -> FilePath -> Files m (Maybe Bool)
  FileExists' :: FilePath -> Files m Bool
  GetAbsPath :: FilePath -> Files m FilePath
  GetDirAbsPath :: Path r Dir -> Files m (Path Abs Dir)
  ListDirRel :: Path a Dir -> Files m ([Path Rel Dir], [Path Rel File])
  PathUid :: Path Abs b -> Files m Uid
  ReadFile' :: FilePath -> Files m Text
  ReadFileBS' :: FilePath -> Files m ByteString
  RemoveDirectoryRecursive' :: Path a Dir -> Files m ()
  WriteFile' :: Path a b -> Text -> Files m ()
  WriteFileBS :: Path a b -> ByteString -> Files m ()

makeSem ''Files
