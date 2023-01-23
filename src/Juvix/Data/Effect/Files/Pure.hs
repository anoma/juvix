module Juvix.Data.Effect.Files.Pure where

import Data.HashMap.Strict qualified as HashMap
import Data.Tree
import Juvix.Data.Effect.Files.Base
import Juvix.Prelude.Base
import Juvix.Prelude.Path
import Prelude qualified

data FS = FS
  { _fsRoot :: Path Abs Dir,
    _fsNode :: FSNode
  }

data FSNode = FSNode
  { _dirFiles :: HashMap (Path Rel File) Text,
    _dirDirs :: HashMap (Path Rel Dir) FSNode
  }

emptyFS :: FS
emptyFS =
  FS
    { _fsRoot = $(mkAbsDir "/"),
      _fsNode = emptyNode
    }

emptyNode :: FSNode
emptyNode =
  FSNode
    { _dirFiles = mempty,
      _dirDirs = mempty
    }

makeLenses ''FS
makeLenses ''FSNode

mkFS :: HashMap (Path Abs File) Text -> FS
mkFS tbl = run (execState emptyFS go)
  where
    go :: Sem '[State FS] ()
    go = forM_ (HashMap.toList tbl) $ \(p, txt) -> do
      ensureDirHelper (parent p)
      writeFileHelper p txt

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

runFilesEmpty :: Sem (Files ': r) a -> Sem r a
runFilesEmpty = runFilesPure mempty $(mkAbsDir "/")

runFilesPure :: HashMap (Path Abs File) Text -> Path Abs Dir -> Sem (Files ': r) a -> Sem r a
runFilesPure ini cwd a = evalState (mkFS ini) (re cwd a)

re :: Path Abs Dir -> Sem (Files ': r) a -> Sem (State FS ': r) a
re cwd = reinterpret $ \case
  ReadFile' f -> lookupFile' f
  FileExists' f -> isJust <$> lookupFile f
  PathUid p -> return (Uid (toFilePath p))
  ReadFileBS' f -> encodeUtf8 <$> lookupFile' f
  GetDirAbsPath p -> return (absDir (cwd' </> toFilePath p))
  EnsureDir' p -> ensureDirHelper p
  DirectoryExists' p -> isJust <$> lookupDir p
  WriteFile' p t -> writeFileHelper p t
  WriteFileBS p t -> writeFileHelper p (decodeUtf8 t)
  RemoveDirectoryRecursive' p -> removeDirRecurHelper p
  ListDirRel p -> do
    n <- lookupDir' p
    return (HashMap.keys (n ^. dirDirs), HashMap.keys (n ^. dirFiles))
  where
    cwd' :: FilePath
    cwd' = toFilePath cwd

missingErr :: (Members '[State FS] r) => FilePath -> Sem r a
missingErr f = do
  root <- get @FS
  error $
    pack $
      "file "
        <> f
        <> " does not exist."
        <> "\nThe contents of the mocked file system are:\n"
        <> Prelude.show root

checkRoot :: (Members '[State FS] r) => Path Abs Dir -> Sem r ()
checkRoot r = do
  root <- gets (^. fsRoot)
  unless True (error ("roots do not match: " <> pack (toFilePath root) <> "\n" <> pack (toFilePath r)))

removeDirRecurHelper :: (Members '[State FS] r) => Path Abs Dir -> Sem r ()
removeDirRecurHelper p = do
  checkRoot r
  modify (over fsNode (fromMaybe emptyNode . go dirs))
  where
    (r, dirs) = destructAbsDir p
    go :: [Path Rel Dir] -> FSNode -> Maybe FSNode
    go = \case
      [] -> const Nothing
      (d : ds) -> Just . over dirDirs (HashMap.alter helper d)
        where
          helper :: Maybe FSNode -> Maybe FSNode
          helper = go ds . fromMaybe emptyNode

ensureDirHelper :: (Members '[State FS] r) => Path Abs Dir -> Sem r ()
ensureDirHelper p = do
  checkRoot r
  modify (over fsNode (go dirs))
  where
    (r, dirs) = destructAbsDir p
    go :: [Path Rel Dir] -> FSNode -> FSNode
    go = \case
      [] -> id
      (d : ds) -> over dirDirs (HashMap.alter (Just . helper) d)
        where
          helper :: Maybe FSNode -> FSNode
          helper = go ds . fromMaybe emptyNode

writeFileHelper :: (Members '[State FS] r) => Path Abs File -> Text -> Sem r ()
writeFileHelper p contents = do
  checkRoot r
  modify (over fsNode (go dirs))
  where
    (r, dirs, f) = destructAbsFile p
    go :: [Path Rel Dir] -> FSNode -> FSNode
    go = \case
      [] -> set (dirFiles . at f) (Just contents)
      (d : ds) -> over dirDirs (HashMap.alter (Just . helper) d)
        where
          helper :: Maybe FSNode -> FSNode
          helper = maybe (error "directory does not exist") (go ds)

lookupDir :: (Members '[State FS] r) => Path Abs Dir -> Sem r (Maybe FSNode)
lookupDir p = do
  checkRoot p
  r <- gets (^. fsNode)
  return (go r (snd (destructAbsDir p)))
  where
    go :: FSNode -> [Path Rel Dir] -> Maybe FSNode
    go d = \case
      [] -> return d
      (h : hs) -> do
        d' <- HashMap.lookup h (d ^. dirDirs)
        go d' hs

lookupDir' :: forall r. (Members '[State FS] r) => Path Abs Dir -> Sem r FSNode
lookupDir' p = fromMaybeM err (lookupDir p)
  where
    err :: Sem r FSNode
    err = missingErr (toFilePath p)

lookupFile :: (Members '[State FS] r) => Path Abs File -> Sem r (Maybe Text)
lookupFile p = do
  node <- lookupDir (parent p)
  return (node >>= HashMap.lookup (filename p) . (^. dirFiles))

lookupFile' :: (Members '[State FS] r) => Path Abs File -> Sem r Text
lookupFile' p =
  fromMaybeM err (lookupFile p)
  where
    err = missingErr (toFilePath p)
