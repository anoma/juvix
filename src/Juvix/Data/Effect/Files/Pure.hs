module Juvix.Data.Effect.Files.Pure where

import Data.HashMap.Strict qualified as HashMap
import Data.Tree
import Juvix.Data.Effect.Files.Base
import Juvix.Prelude.Base
import Path hiding ((</>))
import Path qualified
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

relFile :: FilePath -> Path Rel File
relFile = fromJust . parseRelFile

relDir :: FilePath -> Path Rel Dir
relDir = fromJust . parseRelDir

destructPath :: Path b Dir -> [Path Rel Dir]
destructPath p = map relDir (splitPath (toFilePath p))

destructFilePath :: Path b File -> ([Path Rel Dir], Path Rel File)
destructFilePath p = case nonEmptyUnsnoc (nonEmpty' (splitPath (toFilePath p))) of
  (ps, f) -> (fmap relDir (maybe [] toList ps), relFile f)

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
  RegisterStdlib {} -> return ()
  UpdateStdlib {} -> return ()
  GetAbsPath f -> return (rootPath </> f)
  CanonicalizePath' f -> return (normalise (rootPath </> f))
  ReadFileBS' f -> return (encodeUtf8 (readHelper f))
  where
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
      forM_ dirs' $ \(d, n) -> go (cur Path.</> d) n
      where
        args :: RecursorArgs
        args =
          RecursorArgs
            { _recCurDir = cur,
              _recFiles = HashMap.keys files,
              _recDirs = HashMap.keys dirs
            }
