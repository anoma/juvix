module Juvix.Data.Effect.Files.Base
  ( module Juvix.Data.Effect.Files.Base,
    module Juvix.Data.Uid,
  )
where

import Data.Time
import Juvix.Data.Uid
import Juvix.Prelude.Base
import Juvix.Prelude.Prepath
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

data TempFile :: Effect where
  TempFilePath :: TempFile m (Path Abs File)
  RemoveTempFile :: Path Abs File -> TempFile m ()

makeSem ''TempFile

data Files :: Effect where
  EnsureDir' :: Path Abs Dir -> Files m ()
  DirectoryExists' :: Path Abs Dir -> Files m Bool
  FileExists' :: Path Abs File -> Files m Bool
  ListDirRel :: Path Abs Dir -> Files m ([Path Rel Dir], [Path Rel File])
  PathUid :: Path Abs b -> Files m Uid
  ReadFile' :: Path Abs File -> Files m Text
  ReadFileBS' :: Path Abs File -> Files m ByteString
  RemoveDirectoryRecursive' :: Path Abs Dir -> Files m ()
  WriteFileEnsureLn' :: Path Abs File -> Text -> Files m ()
  WriteFileBS :: Path Abs File -> ByteString -> Files m ()
  RemoveFile' :: Path Abs File -> Files m ()
  RenameFile' :: Path Abs File -> Path Abs File -> Files m ()
  CopyFile' :: Path Abs File -> Path Abs File -> Files m ()
  GetModificationTime' :: Path Abs File -> Files m UTCTime
  JuvixConfigDir :: Files m (Path Abs Dir)
  CanonicalDir :: Path Abs Dir -> Prepath Dir -> Files m (Path Abs Dir)
  NormalizeDir :: Path b Dir -> Files m (Path Abs Dir)
  NormalizeFile :: Path b File -> Files m (Path Abs File)
  FindFile' :: [Path b Dir] -> Path Rel File -> Files m (Maybe (Path Abs File))

makeSem ''Files
