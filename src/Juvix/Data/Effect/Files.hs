module Juvix.Data.Effect.Files
  ( module Juvix.Data.Effect.Files,
    module Juvix.Data.Effect.Files.Error,
    module System.FilePath.Find,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Data.Effect.Files.Error
import Juvix.Extra.Stdlib qualified as Stdlib
import Juvix.Prelude.Base
import System.FilePath.Find (FilterPredicate, FindClause, RecursionPredicate)
import System.FilePath.Find qualified as Find

data Files m a where
  ReadFile' :: FilePath -> Files m Text
  FileExists' :: FilePath -> Files m Bool
  EqualPaths' :: FilePath -> FilePath -> Files m (Maybe Bool)
  GetAbsPath :: FilePath -> Files m FilePath
  RegisterStdlib :: FilePath -> Files m ()
  UpdateStdlib :: FilePath -> Files m ()
  FilesFind :: RecursionPredicate -> FilterPredicate -> FilePath -> Files m [FilePath]

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

-- | Try to resolve a filepath `p` to a path within standard library.
--
-- When `p` is not a member of the standard library or no stanard library is
-- registered, resolve `p` to an absolute path based at `rootPath` instead.
--
-- This function throws an error if `p` is a member of the standard library and
-- also present within `rootPath`.
stdlibOrFile ::
  forall r.
  Members '[Embed IO, Error FilesError] r =>
  FilePath ->
  FilePath ->
  Maybe StdlibState ->
  Sem r FilePath
stdlibOrFile p rootPath m = case m of
  Nothing -> return (rootPath </> p)
  Just s
    | HashSet.member (normalise p) (s ^. stdlibFilePaths) ->
        ifM
          isConflict
          ( throw
              FilesError
                { _filesErrorPath = pAbsPath,
                  _filesErrorCause = StdlibConflict
                }
          )
          (return (s ^. stdlibRoot </> p))
    | otherwise -> return pAbsPath
    where
      pAbsPath :: FilePath
      pAbsPath = rootPath </> p

      isConflict :: Sem r Bool
      isConflict = do
        cRootPath <- embed (canonicalizePath rootPath)
        cStdlibPath <- embed (canonicalizePath (s ^. stdlibRoot))
        andM [return (cRootPath /= cStdlibPath), embed (doesFileExist pAbsPath)]

runFilesIO' ::
  forall r a.
  Member (Embed IO) r =>
  FilePath ->
  Sem (Files ': r) a ->
  Sem (State FilesState ': (Error FilesError ': r)) a
runFilesIO' rootPath = reinterpret2 $ \case
  ReadFile' f -> embed (readFile f)
  FileExists' f -> embed (doesFileExist f)
  EqualPaths' f h -> embed $ do
    f' <- canonicalizePath f
    h' <- canonicalizePath h
    return (Just (equalFilePath f' h'))
  RegisterStdlib stdlibRootPath -> do
    absStdlibRootPath <- embed (makeAbsolute stdlibRootPath)
    fs <- embed (getFilesRecursive absStdlibRootPath)
    let paths = normalise . makeRelative absStdlibRootPath <$> fs
    modify
      ( set
          stdlibState
          ( Just
              StdlibState
                { _stdlibRoot = absStdlibRootPath,
                  _stdlibFilePaths = HashSet.fromList paths
                }
          )
      )
  UpdateStdlib p -> runReader p Stdlib.updateStdlib
  FilesFind re f p -> embed (Find.find re f p)
  GetAbsPath f -> do
    s <- gets (^. stdlibState)
    p <- stdlibOrFile f rootPath s
    embed (canonicalizePath p)

runFilesIO :: Member (Embed IO) r => FilePath -> Sem (Files ': r) a -> Sem (Error FilesError ': r) a
runFilesIO rootPath = evalState initState . runFilesIO' rootPath

runFilesEmpty :: FilePath -> Sem (Files ': r) a -> Sem r a
runFilesEmpty rootPath = runFilesPure rootPath mempty

runFilesPure :: FilePath -> HashMap FilePath Text -> Sem (Files ': r) a -> Sem r a
runFilesPure rootPath fs = interpret $ \case
  ReadFile' f -> case HashMap.lookup f fs of
    Nothing ->
      error $
        pack $
          "file "
            <> f
            <> " does not exist."
            <> "\nThe contents of the mocked file system are:\n"
            <> unlines (HashMap.keys fs)
    Just c -> return c
  EqualPaths' {} -> return Nothing
  FileExists' f -> return (HashMap.member f fs)
  RegisterStdlib {} -> return ()
  UpdateStdlib {} -> return ()
  GetAbsPath f -> return (rootPath </> f)
  FilesFind {} -> error "to be implemented"
