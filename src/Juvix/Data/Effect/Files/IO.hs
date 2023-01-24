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
import System.Posix.Types qualified as P
import System.PosixCompat.Files qualified as P

runFilesIO ::
  forall r a.
  (Member (Embed IO) r) =>
  Sem (Files ': r) a ->
  Sem r a
runFilesIO = interpret helper
  where
    helper :: forall rInitial x. Files (Sem rInitial) x -> Sem r x
    helper = \case
      ReadFile' f -> embed (readFile (toFilePath f))
      WriteFileBS p bs -> embed (ByteString.writeFile (toFilePath p) bs)
      WriteFile' f txt -> embed (writeFile (toFilePath f) txt)
      EnsureDir' p -> Path.ensureDir p
      DirectoryExists' p -> Path.doesDirExist p
      ReadFileBS' f -> embed (ByteString.readFile (toFilePath f))
      FileExists' f -> Path.doesFileExist f
      RemoveDirectoryRecursive' d -> removeDirRecur d
      ListDirRel p -> embed @IO (Path.listDirRel p)
      PathUid f -> do
        status <- embed (P.getFileStatus (toFilePath f))
        let P.CDev dev = P.deviceID status
            P.CIno fid = P.fileID status
        return (Uid (dev, fid))
      GetDirAbsPath f -> canonicalizePath f
