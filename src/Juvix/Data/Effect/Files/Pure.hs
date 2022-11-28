module Juvix.Data.Effect.Files.Pure where

import Data.HashMap.Strict qualified as HashMap
import Data.Tree
import Juvix.Data.Effect.Files.Base
import Juvix.Prelude.Base
import Prelude qualified

data Dir = Dir
  { _dirFiles :: HashMap FilePath Text,
    _dirDirs :: HashMap FilePath Dir
  }

emptyDir :: Dir
emptyDir =
  Dir
    { _dirFiles = mempty,
      _dirDirs = mempty
    }

makeLenses ''Dir

mkDir :: HashMap FilePath Text -> Dir
mkDir = HashMap.foldlWithKey' insertFile emptyDir
  where
    insertFile :: Dir -> FilePath -> Text -> Dir
    insertFile dir0 fp contents = go (splitPath fp) dir0
      where
        go :: [FilePath] -> Dir -> Dir
        go l dir = case l of
          [] -> impossible
          [f] -> set (dirFiles . at f) (Just contents) dir
          (d : ds) -> over dirDirs (HashMap.alter (Just . helper) d) dir
            where
              helper :: Maybe Dir -> Dir
              helper = maybe (helper (Just emptyDir)) (go ds)

toTree :: Dir -> Tree FilePath
toTree d = case (HashMap.toList (d ^. dirDirs), toList (d ^. dirFiles)) of
  ([(r, d')], []) -> Node r (go d')
  _ -> Node "root" (go d)
  where
  go :: Dir -> [Tree FilePath]
  go (Dir files dirs) = map goFile (HashMap.keys files) <> map (uncurry goDir) (HashMap.toList dirs)
  goFile :: FilePath -> Tree FilePath
  goFile f = Node f []
  goDir :: FilePath -> Dir -> Tree FilePath
  goDir p = Node p . go

instance Show Dir where
  show = drawTree . toTree

runFilesEmpty :: FilePath -> Sem (Files ': r) a -> Sem r a
runFilesEmpty rootPath = runFilesPure rootPath mempty

runFilesPure :: FilePath -> HashMap FilePath Text -> Sem (Files ': r) a -> Sem r a
runFilesPure rootPath fs = interpret $ \case
  ReadFile' f -> readHelper f
  EqualPaths' {} -> return Nothing
  FileExists' f -> return (HashMap.member f fs)
  RegisterStdlib {} -> return ()
  UpdateStdlib {} -> return ()
  GetAbsPath f -> return (rootPath </> f)
  CanonicalizePath' f -> return (normalise (rootPath </> f))
  ReadFileBS' f -> encodeUtf8 <$> readHelper f
  where
    dir :: Dir
    dir = mkDir fs
    readHelper :: forall m. Monad m => FilePath -> m Text
    readHelper f = case HashMap.lookup f fs of
      Nothing ->
        error $
          pack $
            "file "
              <> f
              <> " does not exist."
              <> "\nThe contents of the mocked file system are:\n"
              <> Prelude.show dir
      Just c -> return c
