{-# LANGUAGE DeriveGeneric #-}

module MiniJuvix.Syntax.Scoped.Name where

import Data.Stream (Stream (Cons))
import qualified Data.Stream
import qualified MiniJuvix.Syntax.Concrete.Name as C
import MiniJuvix.Utils.Prelude

--------------------------------------------------------------------------------
-- Names
--------------------------------------------------------------------------------

newtype NameId = NameId Word64
  deriving stock (Show, Eq, Ord, Generic)

allNameIds :: Stream NameId
allNameIds = NameId <$> ids
  where
    ids :: Stream Word64
    ids = aux minBound
    aux i = Cons i (aux (succ i))

instance Hashable NameId

type Name = Name' C.Name

type Symbol = Name' C.Symbol

data Name' n = Name'
  { nameId :: NameId,
    nameConcrete :: n,
    nameKind :: NameKind
  }
  deriving stock (Show)

instance Eq (Name' n) where
  (==) = (==) `on` nameId

instance Ord (Name' n) where
  compare = compare `on` nameId

instance Hashable (Name' n) where
  hashWithSalt salt = hashWithSalt salt . nameId

data NameKind
  = -- | Constructor name.
    KNameConstructor
  | -- | Name introduced by the inductive keyword.
    KNameInductive
  | -- | Name of a defined function.
    KNameFunName
  | -- | A locally bound name (patterns, arguments, etc.).
    KNameLocal
  deriving stock (Show)
