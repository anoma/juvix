module Juvix.Data.Effect.Files
  ( module Juvix.Data.Effect.Files,
    module Juvix.Data.Effect.Files.Error,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Data.Effect.Files.Error
import Juvix.Prelude.Base

data Files m a where
  ReadFile' :: FilePath -> Files m Text
  EqualPaths' :: FilePath -> FilePath -> Files m (Maybe Bool)
  RegisterStdlib :: [(FilePath, Text)] -> Files m ()

makeSem ''Files

newtype FilesState = FilesState
  {_stdlibTable :: HashMap FilePath Text}

makeLenses ''FilesState

initState :: FilesState
initState = FilesState mempty

readStdlibOrFile ::
  Members '[Embed IO, Error FilesError] r =>
  FilePath ->
  HashMap FilePath Text ->
  Sem r Text
readStdlibOrFile f stdlib = do
  cf <- embed (canonicalizePath f)
  case HashMap.lookup cf stdlib of
    Nothing -> embed (readFile f)
    Just c -> do
      ifM
        (embed (doesFileExist f))
        ( throw
            FilesError
              { _filesErrorPath = f,
                _filesErrorCause = StdlibConflict
              }
        )
        (return c)

seqFst :: (IO a, b) -> IO (a, b)
seqFst (ma, b) = do
  a <- ma
  return (a, b)

canonicalizeStdlib :: [(FilePath, Text)] -> IO (HashMap FilePath Text)
canonicalizeStdlib stdlib = HashMap.fromList <$> mapM seqFst (first canonicalizePath <$> stdlib)

runFilesIO' :: forall r a. Member (Embed IO) r => Sem (Files ': r) a -> Sem (State FilesState ': (Error FilesError ': r)) a
runFilesIO' = reinterpret2 $ \case
  ReadFile' f -> do
    stdlib <- gets (^. stdlibTable)
    readStdlibOrFile f stdlib
  EqualPaths' f h -> embed $ do
    f' <- canonicalizePath f
    h' <- canonicalizePath h
    return (Just (equalFilePath f' h'))
  RegisterStdlib stdlib -> do
    s <- embed (FilesState <$> canonicalizeStdlib stdlib)
    put s

runFilesIO :: Member (Embed IO) r => Sem (Files ': r) a -> Sem (Error FilesError ': r) a
runFilesIO = evalState initState . runFilesIO'

runFilesEmpty :: Sem (Files ': r) a -> Sem r a
runFilesEmpty = runFilesPure mempty

runFilesPure :: HashMap FilePath Text -> Sem (Files ': r) a -> Sem r a
runFilesPure fs = interpret $ \case
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
