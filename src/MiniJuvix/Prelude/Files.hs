module MiniJuvix.Prelude.Files where

import qualified Data.HashMap.Strict as HashMap
import MiniJuvix.Prelude.Base

data Files m a where
  ReadFile' :: FilePath -> Files m Text
  EqualPaths' :: FilePath -> FilePath -> Files m (Maybe Bool)

makeSem ''Files

runFilesIO :: Member (Embed IO) r => Sem (Files ': r) a -> Sem r a
runFilesIO = interpret $ \case
  ReadFile' f -> embed (readFile f)
  EqualPaths' f h -> embed $ do
    f' <- makeAbsolute f
    h' <- makeAbsolute h
    return (Just $ f' == h')

runFilesEmpty :: Sem (Files ': r) a -> Sem r a
runFilesEmpty = runFilesPure mempty

runFilesPure :: HashMap FilePath Text -> Sem (Files ': r) a -> Sem r a
runFilesPure fs = interpret $ \case
  ReadFile' f -> case HashMap.lookup f fs of
    Nothing ->
      error $
        pack $
          "file " <> f <> " does not exist."
            <> "\nThe contents of the mocked file system are:\n"
            <> unlines (HashMap.keys fs)
    Just c -> return c
  EqualPaths' _ _ -> return Nothing
