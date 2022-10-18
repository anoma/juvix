module Juvix.Data.Effect.Files
  ( module Juvix.Data.Effect.Files,
    module Juvix.Data.Effect.Files.Error,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Data.Effect.Files.Error
import Juvix.Prelude.Base

data Files m a where
  ReadFile' :: FilePath -> Files m Text
  EqualPaths' :: FilePath -> FilePath -> Files m (Maybe Bool)
  GetAbsPath :: FilePath -> Files m FilePath
  RegisterStdlib :: FilePath -> Files m ()

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

stdlibOrFile ::
  forall r.
  Members '[Embed IO, Error FilesError] r =>
  FilePath ->
  FilePath ->
  Maybe StdlibState ->
  Sem r FilePath
stdlibOrFile p rootPath Nothing = return (rootPath </> p)
stdlibOrFile p rootPath (Just s)
  | HashSet.member (normalise p) (s ^. stdlibFilePaths) =
      ifM
        isConflict
        ( throw
            FilesError
              { _filesErrorPath = pAbsPath,
                _filesErrorCause = StdlibConflict
              }
        )
        (return (s ^. stdlibRoot </> p))
  | otherwise = return pAbsPath
  where
    pAbsPath :: FilePath
    pAbsPath = rootPath </> p

    isConflict :: Sem r Bool
    isConflict = do
      cRootPath <- embed (canonicalizePath rootPath)
      cStdlibPath <- embed (canonicalizePath (s ^. stdlibRoot))
      andM [return (cRootPath /= cStdlibPath), embed (doesFileExist pAbsPath)]

seqFst :: (IO a, b) -> IO (a, b)
seqFst (ma, b) = do
  a <- ma
  return (a, b)

runFilesIO' ::
  forall r a.
  Member (Embed IO) r =>
  FilePath ->
  Sem (Files ': r) a ->
  Sem (State FilesState ': (Error FilesError ': r)) a
runFilesIO' rootPath = reinterpret2 $ \case
  ReadFile' f -> embed (readFile f)
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
                  _stdlibFilePaths = HashSet.fromList (normalise <$> paths)
                }
          )
      )
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
  EqualPaths' _ _ -> return Nothing
  RegisterStdlib {} -> return ()
  GetAbsPath f -> return (rootPath </> f)
