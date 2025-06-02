module Juvix.Compiler.Concrete.Data.Name where

import Data.List.NonEmpty.Extra qualified as NonEmpty
import Juvix.Data.Fixity
import Juvix.Data.Loc
import Juvix.Data.TopModulePathKey
import Juvix.Data.WithLoc
import Juvix.Extra.Serialize
import Juvix.Prelude.Base
import Juvix.Prelude.Pretty as Pretty

type Symbol = WithLoc Text

symbolText :: Lens' Symbol Text
symbolText = withLocParam

symbolLoc :: Lens' Symbol Interval
symbolLoc = withLocInt

data Name
  = NameQualified QualifiedName
  | NameUnqualified Symbol
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize Name

instance NFData Name

instance HasLoc Name where
  getLoc = \case
    NameQualified q -> getLoc q
    NameUnqualified s -> getLoc s

instance HasAtomicity Name where
  atomicity = const Atom

instance Pretty QualifiedName where
  pretty (QualifiedName (SymbolPath path) s) =
    let symbols = snoc (toList path) s
     in dotted (map pretty symbols)
    where
      dotted :: (Foldable f) => f (Doc a) -> Doc a
      dotted = concatWith (surround ".")

instance Pretty Name where
  pretty = \case
    NameQualified q -> pretty q
    NameUnqualified s -> pretty s

newtype SymbolPath = SymbolPath
  { _pathParts :: NonEmpty Symbol
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize SymbolPath

instance NFData SymbolPath

data QualifiedName = QualifiedName
  { _qualifiedPath :: SymbolPath,
    _qualifiedSymbol :: Symbol
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize QualifiedName

instance NFData QualifiedName

instance HasLoc QualifiedName where
  getLoc QualifiedName {..} =
    getLoc _qualifiedPath <> getLoc _qualifiedSymbol

instance Hashable QualifiedName

instance HasLoc SymbolPath where
  getLoc (SymbolPath p) = getLoc (NonEmpty.head p) <> getLoc (NonEmpty.last p)

deriving newtype instance Hashable SymbolPath

makePrisms ''Name
makeLenses ''QualifiedName
makeLenses ''SymbolPath

-- | A.B.C corresponds to TopModulePath [A,B] C
data TopModulePath = TopModulePath
  { _modulePathDir :: [Symbol],
    _modulePathName :: Symbol
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize TopModulePath

instance NFData TopModulePath

instance Hashable TopModulePath

data AbsModulePath = AbsModulePath
  { _absTopModulePath :: TopModulePath,
    -- | List of local module names
    _absLocalPath :: [Symbol]
  }
  deriving stock (Show, Eq, Generic)

instance Serialize AbsModulePath

instance NFData AbsModulePath

makeLenses ''TopModulePath

makeLenses ''AbsModulePath

instance HasLoc AbsModulePath where
  getLoc a = getLoc (a ^. absTopModulePath)

topModulePathToAbsPath :: TopModulePath -> AbsModulePath
topModulePathToAbsPath p =
  AbsModulePath
    { _absTopModulePath = p,
      _absLocalPath = []
    }

instance Hashable AbsModulePath

-- | Tells whether the first argument is an immediate child of the second argument.
-- In other words, tells whether the first argument is a local module of the second.
isChildOf :: AbsModulePath -> AbsModulePath -> Bool
isChildOf child parentMod
  | null (child ^. absLocalPath) = False
  | otherwise =
      init (child ^. absLocalPath) == parentMod ^. absLocalPath
        && child ^. absTopModulePath == parentMod ^. absTopModulePath

-- | Appends a local path to the absolute path
-- e.g. TopMod.Local <.> Inner == TopMod.Local.Inner
appendModulePath :: AbsModulePath -> Symbol -> AbsModulePath
appendModulePath absP localMod = absP {_absLocalPath = absP ^. absLocalPath ++ [localMod]}

topModulePathKey :: TopModulePath -> TopModulePathKey
topModulePathKey TopModulePath {..} =
  TopModulePathKey
    { _modulePathKeyDir = (^. symbolText) <$> _modulePathDir,
      _modulePathKeyName = _modulePathName ^. symbolText
    }

instance Pretty TopModulePath where
  pretty = pretty . topModulePathKey

instance HasLoc TopModulePath where
  getLoc TopModulePath {..} =
    case _modulePathDir of
      [] -> getLoc _modulePathName
      (x : _) -> getLoc x <> getLoc _modulePathName

mkTopModulePath :: NonEmpty Symbol -> TopModulePath
mkTopModulePath l = TopModulePath (NonEmpty.init l) (NonEmpty.last l)

topModulePathToName :: TopModulePath -> Name
topModulePathToName (TopModulePath ms m) = case nonEmpty ms of
  Nothing -> NameUnqualified m
  Just ms' -> NameQualified (QualifiedName (SymbolPath ms') m)

topModulePathToDottedPath :: (IsString s) => TopModulePath -> s
topModulePathToDottedPath = fromText . mconcat . intersperse "." . toList . topModulePathParts

topModulePathParts :: TopModulePath -> NonEmpty Text
topModulePathParts TopModulePath {..} = (^. withLocParam) <$> prependList _modulePathDir (pure _modulePathName)

moduleNameToTopModulePath :: Name -> TopModulePath
moduleNameToTopModulePath = \case
  NameUnqualified s -> TopModulePath [] s
  NameQualified (QualifiedName (SymbolPath p) s) -> TopModulePath (toList p) s

fromUnqualified' :: Name -> Symbol
fromUnqualified' = \case
  NameUnqualified s -> s
  NameQualified {} -> impossible

splitName :: Name -> ([Symbol], Symbol)
splitName = \case
  NameQualified (QualifiedName (SymbolPath p) s) -> (toList p, s)
  NameUnqualified s -> ([], s)
