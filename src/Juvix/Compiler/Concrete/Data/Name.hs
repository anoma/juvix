module Juvix.Compiler.Concrete.Data.Name where

import Data.List.NonEmpty.Extra qualified as NonEmpty
import Juvix.Prelude
import Juvix.Prelude.Pretty as Pretty

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
  getLoc = \case
    NameQualified q -> getLoc q
    NameUnqualified s -> getLoc s

instance Pretty QualifiedName where
  pretty (QualifiedName (Path path) s) =
    let symbols = snoc (toList path) s
     in dotted (map pretty symbols)
    where
      dotted :: Foldable f => f (Doc a) -> Doc a
      dotted = concatWith (surround ".")

instance Pretty Name where
  pretty = \case
    NameQualified q -> pretty q
    NameUnqualified s -> pretty s

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

instance Pretty TopModulePath where
  pretty (TopModulePath path name) =
    mconcat (punctuate Pretty.dot (map pretty (snoc path name)))

instance HasLoc TopModulePath where
  getLoc TopModulePath {..} =
    case _modulePathDir of
      [] -> getLoc _modulePathName
      (x : _) -> getLoc x <> getLoc _modulePathName

topModulePathToFilePath :: Member Files r => TopModulePath -> Sem r FilePath
topModulePathToFilePath = topModulePathToFilePath' (Just ".juvix")

topModulePathToRelativeFilePath :: Maybe String -> String -> (FilePath -> FilePath -> FilePath) -> TopModulePath -> FilePath
topModulePathToRelativeFilePath ext suffix joinpath mp = relFilePath
  where
    relDirPath :: FilePath
    relDirPath = foldr (joinpath . toPath) mempty (mp ^. modulePathDir)
    relFilePath :: FilePath
    relFilePath = addExt (relDirPath `joinpath'` toPath (mp ^. modulePathName) <> suffix)
    joinpath' :: FilePath -> FilePath -> FilePath
    joinpath' l r
      | null l = r
      | otherwise = joinpath l r
    addExt = case ext of
      Nothing -> id
      Just e -> (<.> e)
    toPath :: Symbol -> FilePath
    toPath s = unpack (s ^. symbolText)

topModulePathToFilePath' ::
  Member Files r => Maybe String -> TopModulePath -> Sem r FilePath
topModulePathToFilePath' ext mp =
  getAbsPath (topModulePathToRelativeFilePath ext "" (</>) mp)

topModulePathToDottedPath :: IsString s => TopModulePath -> s
topModulePathToDottedPath (TopModulePath l r) =
  fromText $ mconcat $ intersperse "." $ map (^. symbolText) $ l ++ [r]

instance Hashable TopModulePath
