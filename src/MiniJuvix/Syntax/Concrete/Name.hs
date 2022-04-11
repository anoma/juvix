module MiniJuvix.Syntax.Concrete.Name where

import Data.List.NonEmpty.Extra qualified as NonEmpty
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Loc
import Prettyprinter

data Symbol = Symbol
  { _symbolText :: Text,
    _symbolLoc :: Interval
  }
  deriving stock (Show)

instance Pretty Symbol where
  pretty = pretty . _symbolText

instance Eq Symbol where
  (==) = (==) `on` _symbolText

instance Ord Symbol where
  compare = compare `on` _symbolText

instance HasLoc Symbol where
  getLoc = _symbolLoc

instance Hashable Symbol where
  hashWithSalt i Symbol {..} = hashWithSalt i _symbolText

data Name
  = NameQualified QualifiedName
  | NameUnqualified Symbol
  deriving stock (Show, Eq, Ord)

instance HasLoc Name where
  getLoc n = case n of
    NameQualified q -> getLoc q
    NameUnqualified s -> getLoc s

newtype Path = Path
  { pathParts :: NonEmpty Symbol
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

-- | A.B.C corresponds to TopModulePath [A,B] C
data TopModulePath = TopModulePath
  { _modulePathDir :: [Symbol],
    _modulePathName :: Symbol
  }
  deriving stock (Show, Eq, Ord, Generic)

instance HasLoc TopModulePath where
  getLoc TopModulePath {..} =
    case _modulePathDir of
      [] -> getLoc _modulePathName
      (x : _) -> getLoc x <> getLoc _modulePathName

topModulePathToFilePath :: FilePath -> TopModulePath -> FilePath
topModulePathToFilePath = topModulePathToFilePath' (Just ".mjuvix")

topModulePathToFilePath' ::
  Maybe String -> FilePath -> TopModulePath -> FilePath
topModulePathToFilePath' ext root mp = absPath
  where
    relDirPath = foldr ((</>) . toPath) mempty (_modulePathDir mp)
    relFilePath = relDirPath </> toPath (_modulePathName mp)
    absPath = case ext of
      Nothing -> root </> relFilePath
      Just e -> root </> relFilePath <.> e
    toPath :: Symbol -> FilePath
    toPath Symbol {..} = unpack _symbolText

topModulePathToDottedPath :: IsString s => TopModulePath -> s
topModulePathToDottedPath (TopModulePath l r) =
  fromText $ mconcat $ intersperse "." $ map fromSymbol $ l ++ [r]
  where
    fromSymbol Symbol {..} = _symbolText

instance Hashable TopModulePath

makeLenses ''Symbol
