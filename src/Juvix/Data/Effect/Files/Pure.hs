module Juvix.Data.Effect.Files.Pure where

import Data.HashMap.Strict qualified as HashMap
import Data.Tree
import Juvix.Data.Effect.Files.Base
import Juvix.Prelude.Base
import Juvix.Prelude.Path
import Prelude qualified

data FS = FS
  { _fsRoot :: Path Rel Dir,
    _fsNode :: FSNode
  }

data FSNode = FSNode
  { _dirFiles :: HashMap (Path Rel File) Text,
    _dirDirs :: HashMap (Path Rel Dir) FSNode
  }

emptyNode :: FSNode
emptyNode =
  FSNode
    { _dirFiles = mempty,
      _dirDirs = mempty
    }

makeLenses ''FS
makeLenses ''FSNode

-- | Files should have a common root
mkFS :: HashMap FilePath Text -> FS
mkFS tbl = case (HashMap.toList (rootNode ^. dirDirs), toList (rootNode ^. dirFiles)) of
  ([(r, d')], []) -> FS r d'
  _ -> impossible
  where
    rootNode :: FSNode
    rootNode = HashMap.foldlWithKey' insertFile emptyNode tbl
    insertFile :: FSNode -> FilePath -> Text -> FSNode
    insertFile dir0 fp contents = go (splitPath fp) dir0
      where
        go :: [FilePath] -> FSNode -> FSNode
        go l dir = case l of
          [] -> impossible
          [f] -> set (dirFiles . at (relFile f)) (Just contents) dir
          (d : ds) -> over dirDirs (HashMap.alter (Just . helper) (relDir d)) dir
            where
              helper :: Maybe FSNode -> FSNode
              helper = maybe (helper (Just emptyNode)) (go ds)

toTree :: FS -> Tree FilePath
toTree fs = Node (toFilePath (fs ^. fsRoot)) (go (fs ^. fsNode))
  where
    go :: FSNode -> [Tree FilePath]
    go (FSNode files dirs) =
      map goFile (HashMap.keys files)
        <> map (uncurry goNode) (HashMap.toList dirs)
    goFile :: Path Rel File -> Tree FilePath
    goFile f = Node (toFilePath f) []
    goNode :: Path Rel Dir -> FSNode -> Tree FilePath
    goNode p = Node (toFilePath p) . go

instance Show FS where
  show = drawTree . toTree

runFilesEmpty :: FilePath -> Sem (Files ': r) a -> Sem r a
runFilesEmpty rootPath = runFilesPure rootPath mempty

runFilesPure :: FilePath -> HashMap FilePath Text -> Sem (Files ': r) a -> Sem r a
runFilesPure rootPath fs = interpret $ \case
  ReadFile' f -> return (readHelper f)
  EqualPaths' {} -> return Nothing
  FileExists' f -> return (HashMap.member f fs)
  GetAbsPath f -> return (rootPath </> f)
  CanonicalizePath' f -> return (canonicalized f)
  PathUid p -> return (Uid (toFilePath p))
  ReadFileBS' f -> return (encodeUtf8 (readHelper f))
  where
    canonicalized f = normalise (rootPath </> f)
    root :: FS
    root = mkFS fs
    readHelper :: FilePath -> Text
    readHelper f = fromMaybe (missingErr root f) (HashMap.lookup f fs)

missingErr :: FS -> FilePath -> a
missingErr root f =
  error $
    pack $
      "file "
        <> f
        <> " does not exist."
        <> "\nThe contents of the mocked file system are:\n"
        <> Prelude.show root

findDir :: FS -> Path Rel Dir -> FSNode
findDir root p = go (root ^. fsNode) (destructPath p)
  where
    go :: FSNode -> [Path Rel Dir] -> FSNode
    go d = \case
      [] -> d
      (h : hs) -> go (HashMap.lookupDefault err h (d ^. dirDirs)) hs
    err :: a
    err = missingErr root (toFilePath p)

walkDirRel' ::
  forall m.
  Monad m =>
  FS ->
  (RecursorArgs -> m (Path Rel Dir -> Bool)) ->
  Maybe (Path Rel Dir) ->
  m ()
walkDirRel' root f start = go (root ^. fsRoot) fs0
  where
    fs0 :: FSNode
    fs0 = maybe (root ^. fsNode) (findDir root) start
    go :: Path Rel Dir -> FSNode -> m ()
    go cur (FSNode files dirs) = do
      w <- f args
      let dirs' = filter (w . fst) (HashMap.toList dirs)
      forM_ dirs' $ \(d, n) -> go (cur <//> d) n
      where
        args :: RecursorArgs
        args =
          RecursorArgs
            { _recCurDir = cur,
              _recFiles = HashMap.keys files,
              _recDirs = HashMap.keys dirs
            }
