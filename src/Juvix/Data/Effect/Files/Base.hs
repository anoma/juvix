module Juvix.Data.Effect.Files.Base
  ( module Juvix.Data.Effect.Files.Base,
    module Juvix.Data.Effect.Files.Error,
  )
where

import Juvix.Data.Effect.Files.Error
import Juvix.Prelude.Base
import Path

data RecursorArgs = RecursorArgs
  { _recCurDir :: Path Rel Dir,
    _recDirs :: [Path Rel Dir],
    _recFiles :: [Path Rel File]
  }

data Recurse r =
  RecurseNever
  | RecurseFilter (Path r Dir -> Bool)

makeLenses ''RecursorArgs

data Files m a where
  ReadFile' :: FilePath -> Files m Text
  ReadFileBS' :: FilePath -> Files m ByteString
  FileExists' :: FilePath -> Files m Bool
  EqualPaths' :: FilePath -> FilePath -> Files m (Maybe Bool)
  GetAbsPath :: FilePath -> Files m FilePath
  GetDirAbsPath :: Path r Dir -> Files m (Path Abs Dir)
  CanonicalizePath' :: FilePath -> Files m FilePath
  HashPath :: Path a b -> Files m Int
  ListDirRel :: Path a Dir -> Files m ([Path Rel Dir], [Path Rel File])

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
