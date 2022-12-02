module Juvix.Data.Effect.Files.IO
  ( module Juvix.Data.Effect.Files.IO,
    module Juvix.Data.Effect.Files.Base,
  )
where

import Data.ByteString qualified as ByteString
import Juvix.Data.Effect.Files.Base
import Juvix.Prelude.Base
import Juvix.Prelude.Path
import System.Posix.Types qualified as P
import System.PosixCompat.Files qualified as P
import Path.IO qualified as Path

runFilesIO' ::
  forall r a r'.
  ( Member (Embed IO) r,
    r' ~ State FilesState ': Error FilesError ': r) =>
  FilePath ->
  Sem (Files ': r) a ->
  Sem r' a
runFilesIO' rootPath = reinterpret2 helper
  where
    helper :: forall rInitial x. Files (Sem rInitial) x -> Sem r' x
    helper = \case
      ReadFile' f -> embed (readFile f)
      ReadFileBS' f -> embed (ByteString.readFile f)
      FileExists' f -> embed (doesFileExist f)
      ListDirRel p -> embed @IO (Path.listDirRel p)
      EqualPaths' f h ->
        embed
          ( do
              f' <- canonicalizePath f
              h' <- canonicalizePath h
              return (Just (equalFilePath f' h'))
          )
      CanonicalizePath' f -> embed (canonicalizePath f)
      HashPath f -> do
        status <- embed (P.getFileStatus (toFilePath f))
        let P.CDev dev = P.deviceID status
            P.CIno fid = P.fileID status
        return (hash (dev, fid))
      GetDirAbsPath f -> absDir <$> embed (canonicalizePath (rootPath </> toFilePath f))
      GetAbsPath p -> embed (canonicalizePath (rootPath </> p))

runFilesIO :: Member (Embed IO) r => FilePath -> Sem (Files ': r) a -> Sem (Error FilesError ': r) a
runFilesIO rootPath = evalState initState . runFilesIO' rootPath

