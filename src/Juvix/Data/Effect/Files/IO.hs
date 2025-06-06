module Juvix.Data.Effect.Files.IO
  ( module Juvix.Data.Effect.Files.IO,
    module Juvix.Data.Effect.Files.Base,
  )
where

import Control.Monad.Catch qualified as MC
import Data.ByteString qualified as ByteString
import Juvix.Data.Effect.Files.Base
import Juvix.Extra.Version
import Juvix.Prelude.Base
import Juvix.Prelude.Path
import Juvix.Prelude.Prepath
import Path.IO qualified as Path
import System.Environment.XDG.BaseDir
import System.IO.Temp
import System.Posix.Types qualified as P
import System.PosixCompat.Files qualified as P

runFilesIO ::
  forall r a.
  (Members '[EmbedIO] r) =>
  Sem (Files ': r) a ->
  Sem r a
runFilesIO = interpret helper
  where
    helper :: forall rInitial x. Files (Sem rInitial) x -> Sem r x
    helper = liftIO . helper'

    helper' :: forall rInitial x. Files (Sem rInitial) x -> IO x
    helper' = \case
      ReadFile' f -> readFile f
      WriteFileBS p bs -> ByteString.writeFile (toFilePath p) bs
      WriteFileEnsureLn' f txt -> writeFileEnsureLn f txt
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
      RemoveFile' f -> Path.removeFile f
      RenameFile' p1 p2 -> Path.renameFile p1 p2
      CopyFile' p1 p2 -> Path.copyFile p1 p2
      CopyDirectory p1 p2 -> Path.copyDirRecur p1 p2
      JuvixConfigDir -> juvixConfigDirIO >>= canonicalizePath
      CanonicalDir root d -> prepathToAbsDir root d
      NormalizeDir p -> canonicalizePath p
      NormalizeFile b -> canonicalizePath b
      FindFile' possiblePaths f -> Path.findFile possiblePaths f

juvixConfigDirIO :: IO (Path Abs Dir)
juvixConfigDirIO = (<//> versionDir) . absDir <$> getUserConfigDir "juvix"

runTempFileIO ::
  forall r a.
  (Members '[EmbedIO] r) =>
  Sem (TempFile ': r) a ->
  Sem r a
runTempFileIO = interpret $ \case
  TempFilePath -> liftIO (emptySystemTempFile "tmp" >>= parseAbsFile)
  RemoveTempFile p -> liftIO (ignoringIOErrors (Path.removeFile p))
    where
      ignoringIOErrors :: IO () -> IO ()
      ignoringIOErrors ioe = MC.catch ioe (\(_ :: IOError) -> return ())
