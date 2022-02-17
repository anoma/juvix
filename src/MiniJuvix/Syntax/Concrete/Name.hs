{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MiniJuvix.Syntax.Concrete.Name where

import Language.Haskell.TH.Syntax (Lift)
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Loc
import qualified Data.List.NonEmpty.Extra as NonEmpty

data Symbol = Symbol {
  _symbolText :: Text,
  _symbolLoc :: Interval
  }
  deriving stock (Show, Lift)

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
  deriving stock (Show, Eq, Ord, Lift)

instance HasLoc Name where
  getLoc n = case n of
    NameQualified q -> getLoc q
    NameUnqualified s -> getLoc s

newtype Path = Path
  { pathParts :: NonEmpty Symbol
  }
  deriving stock (Show, Eq, Ord, Lift)

data QualifiedName = QualifiedName
  { _qualifiedPath :: Path,
    _qualifiedSymbol :: Symbol
  }
  deriving stock (Show, Eq, Ord, Generic, Lift)

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
  { modulePathDir :: [Symbol],
    modulePathName :: Symbol
  }
  deriving stock (Show, Eq, Ord, Generic, Lift)

instance HasLoc TopModulePath where
  getLoc TopModulePath {..} =
    case modulePathDir of
      [] -> getLoc modulePathName
      (x : _) -> getLoc x <> getLoc modulePathName

topModulePathToFilePath :: FilePath -> TopModulePath -> FilePath
topModulePathToFilePath = topModulePathToFilePath' (Just ".mjuvix")

topModulePathToFilePath'
  :: Maybe String -> FilePath -> TopModulePath -> FilePath
topModulePathToFilePath' ext root mp = absPath
  where
  relDirPath = foldr ((</>) . toPath) mempty (modulePathDir mp)
  relFilePath = relDirPath </> toPath (modulePathName mp)
  absPath = case ext of
    Nothing -> root </> relFilePath
    Just e -> root </> relFilePath <.> e
  toPath :: Symbol -> FilePath
  toPath Symbol{..} = unpack _symbolText

instance Hashable TopModulePath
