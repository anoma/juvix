module Juvix.Syntax.Concrete.Name where

import Data.List.NonEmpty.Extra qualified as NonEmpty
import Juvix.Prelude
import Juvix.Syntax.Loc

type Symbol = WithLoc Text

symbolText :: Lens' Symbol Text
symbolText = withLocParam

symbolLoc :: Lens' Symbol Interval
symbolLoc = withLocInt

data Name
  = NameQualified QualifiedName
  | NameUnqualified Symbol
  deriving stock (Show, Eq, Ord)

instance HasLoc Name where
  getLoc n = case n of
    NameQualified q -> getLoc q
    NameUnqualified s -> getLoc s

newtype Path = Path
  { _pathParts :: NonEmpty Symbol
  }
  deriving stock (Show, Eq, Ord)

data QualifiedName = QualifiedName
  { _qualifiedPath :: Path,
    _qualifiedSymbol :: Symbol
  }
  deriving stock (Show, Eq, Ord, Generic)

instance HasLoc QualifiedName where
  getLoc QualifiedName {..} =
    getLoc _qualifiedPath <> getLoc _qualifiedSymbol

instance Hashable QualifiedName

instance HasLoc Path where
  getLoc (Path p) = getLoc (NonEmpty.head p) <> getLoc (NonEmpty.last p)

deriving newtype instance Hashable Path

makeLenses ''QualifiedName
makeLenses ''Path

-- | A.B.C corresponds to TopModulePath [A,B] C
data TopModulePath = TopModulePath
  { _modulePathDir :: [Symbol],
    _modulePathName :: Symbol
  }
  deriving stock (Show, Eq, Ord, Generic)

makeLenses ''TopModulePath

instance HasLoc TopModulePath where
  getLoc TopModulePath {..} =
    case _modulePathDir of
      [] -> getLoc _modulePathName
      (x : _) -> getLoc x <> getLoc _modulePathName

topModulePathToFilePath :: FilePath -> TopModulePath -> FilePath
topModulePathToFilePath = topModulePathToFilePath' (Just ".juvix")

topModulePathToFilePath' ::
  Maybe String -> FilePath -> TopModulePath -> FilePath
topModulePathToFilePath' ext root mp = normalise absPath
  where
    relDirPath = foldr ((</>) . toPath) mempty (mp ^. modulePathDir)
    relFilePath = relDirPath </> toPath (mp ^. modulePathName)
    absPath = case ext of
      Nothing -> root </> relFilePath
      Just e -> root </> relFilePath <.> e
    toPath :: Symbol -> FilePath
    toPath s = unpack (s ^. withLocParam)

topModulePathToDottedPath :: IsString s => TopModulePath -> s
topModulePathToDottedPath (TopModulePath l r) =
  fromText $ mconcat $ intersperse "." $ map (^. symbolText) $ l ++ [r]

instance Hashable TopModulePath
