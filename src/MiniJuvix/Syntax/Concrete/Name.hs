{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MiniJuvix.Syntax.Concrete.Name where

import Language.Haskell.TH.Syntax (Lift)
import MiniJuvix.Utils.Prelude
import MiniJuvix.Syntax.Concrete.Loc

data Symbol = Symbol {
  _symbolText :: Text,
  _symbolLoc :: Interval
  }
  deriving stock (Show, Lift)

instance Eq Symbol where
  (==) = (==) `on` _symbolText

instance Ord Symbol where
  compare = compare `on` _symbolText

instance Hashable Symbol where
  hashWithSalt i Symbol {..} = hashWithSalt i _symbolText 

data QualifiedName = QualifiedName
  { qualifiedPath :: Path,
    qualifiedSymbol :: Symbol
  }
  deriving stock (Show, Eq, Ord, Generic, Lift)

instance Hashable QualifiedName

data Name
  = NameQualified QualifiedName
  | NameUnqualified Symbol
  deriving stock (Show, Eq, Ord, Lift)

newtype Path = Path
  { pathParts :: NonEmpty Symbol
  }
  deriving stock (Show, Eq, Ord, Lift)

deriving newtype instance Hashable Path

-- | A.B.C corresponds to TopModulePath [A,B] C
data TopModulePath = TopModulePath
  { modulePathDir :: [Symbol],
    modulePathName :: Symbol
  }
  deriving stock (Show, Eq, Ord, Generic, Lift)

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
