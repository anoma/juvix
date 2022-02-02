{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MiniJuvix.Syntax.Concrete.Name where

import Language.Haskell.TH.Syntax (Lift)
import MiniJuvix.Utils.Prelude

newtype Symbol = Sym Text
  deriving stock (Show, Eq, Ord, Lift)

instance Hashable Symbol where
  hashWithSalt i (Sym t) = hashWithSalt i t

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

instance Hashable TopModulePath
