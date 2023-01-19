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
  pretty (QualifiedName (SymbolPath path) s) =
    let symbols = snoc (toList path) s
     in dotted (map pretty symbols)
    where
      dotted :: Foldable f => f (Doc a) -> Doc a
      dotted = concatWith (surround ".")

instance Pretty Name where
  pretty = \case
    NameQualified q -> pretty q
    NameUnqualified s -> pretty s

newtype SymbolPath = SymbolPath
  { _pathParts :: NonEmpty Symbol
  }
  deriving stock (Show, Eq, Ord)

data QualifiedName = QualifiedName
  { _qualifiedPath :: SymbolPath,
    _qualifiedSymbol :: Symbol
  }
  deriving stock (Show, Eq, Ord, Generic)

instance HasLoc QualifiedName where
  getLoc QualifiedName {..} =
    getLoc _qualifiedPath <> getLoc _qualifiedSymbol

instance Hashable QualifiedName

instance HasLoc SymbolPath where
  getLoc (SymbolPath p) = getLoc (NonEmpty.head p) <> getLoc (NonEmpty.last p)

deriving newtype instance Hashable SymbolPath

makeLenses ''QualifiedName
makeLenses ''SymbolPath

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

topModulePathToName :: TopModulePath -> Name
topModulePathToName (TopModulePath ms m) = case nonEmpty ms of
  Nothing -> NameUnqualified m
  Just ms' -> NameQualified (QualifiedName (SymbolPath ms') m)

topModulePathToDottedPath :: IsString s => TopModulePath -> s
topModulePathToDottedPath (TopModulePath l r) =
  fromText $ mconcat $ intersperse "." $ map (^. symbolText) $ l ++ [r]

instance Hashable TopModulePath
