module MiniJuvix.Syntax.Concrete.Scoped.Scoper.Files where

import MiniJuvix.Prelude
import qualified Data.HashMap.Strict as HashMap

data Files m a where
  ReadFile' :: FilePath -> Files m Text

makeSem ''Files

runFilesIO :: Member (Embed IO) r => Sem (Files ': r) a -> Sem r a
runFilesIO = interpret $ \(ReadFile' f) -> embed (readFile f)

runFilesPure :: HashMap FilePath Text -> Sem (Files ': r) a -> Sem r a
runFilesPure fs = interpret $ \(ReadFile' f) -> case HashMap.lookup f fs of
  Nothing -> error $ pack $ "file " <> f <> " does not exist." <>
    "\nThe contents of the mocked file system are:\n" <>
    unlines (HashMap.keys fs)
  Just c -> return c
