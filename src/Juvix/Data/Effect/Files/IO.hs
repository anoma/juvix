module Juvix.Data.Effect.Files.IO
  ( module Juvix.Data.Effect.Files.IO,
    module Juvix.Data.Effect.Files.Base,
  )
where

import Data.ByteString qualified as ByteString
import Juvix.Data.Effect.Files.Base
import Juvix.Prelude.Base
import Juvix.Prelude.Path
import Path.IO qualified as Path
import System.IO.Error
import System.Posix.Types qualified as P
import System.PosixCompat.Files qualified as P

runFilesIO ::
  forall r a.
  (Members '[Error IOError, Embed IO] r) =>
  Sem (Files ': r) a ->
  Sem r a
runFilesIO = interpret helper
  where
    helper :: forall rInitial x. Files (Sem rInitial) x -> Sem r x
    helper = fromException @IOError . helper'

    helper' :: forall rInitial x. Files (Sem rInitial) x -> IO x
    helper' = \case
      ReadFile' f -> readFile (toFilePath f)
      WriteFileBS p bs -> ByteString.writeFile (toFilePath p) bs
      WriteFile' f txt -> writeFile (toFilePath f) txt
      EnsureDir' p -> Path.ensureDir p
      DirectoryExists' p -> Path.doesDirExist p
      ReadFileBS' f -> ByteString.readFile (toFilePath f)
      FileExists' f -> Path.doesFileExist f
      RemoveDirectoryRecursive' d -> removeDirRecur d
      ListDirRel p -> Path.listDirRel p
      PathUid f -> do
        status <- P.getFileStatus (toFilePath f)
        let P.CDev dev = P.deviceID status
            P.CIno fid = P.fileID status
        return (Uid (dev, fid))
      GetDirAbsPath f -> canonicalizePath f

runIOErrorToIO ::
  forall r a.
  Member (Embed IO) r =>
  Sem (Error IOError ': r) a ->
  Sem r a
runIOErrorToIO a = do
  err <- runError a
  case err of
    Left e -> do
      embed @IO (throwM e)
    Right x -> return x
