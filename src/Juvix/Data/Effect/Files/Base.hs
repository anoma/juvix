module Juvix.Data.Effect.Files.Base
  ( module Juvix.Data.Effect.Files.Base,
    module Juvix.Data.Effect.Files.Error,
  )
where

import Juvix.Data.Effect.Files.Error
import Juvix.Prelude.Base

data Files m a where
  ReadFile' :: FilePath -> Files m Text
  ReadFileBS' :: FilePath -> Files m ByteString
  FileExists' :: FilePath -> Files m Bool
  EqualPaths' :: FilePath -> FilePath -> Files m (Maybe Bool)
  GetAbsPath :: FilePath -> Files m FilePath
  CanonicalizePath' :: FilePath -> Files m FilePath
  RegisterStdlib :: FilePath -> Files m ()
  UpdateStdlib :: FilePath -> Files m ()

makeSem ''Files

data StdlibState = StdlibState
  { _stdlibRoot :: FilePath,
    _stdlibFilePaths :: HashSet FilePath
  }

newtype FilesState = FilesState
  { _stdlibState :: Maybe StdlibState
  }

makeLenses ''FilesState
makeLenses ''StdlibState

initState :: FilesState
initState = FilesState Nothing

