{-# LANGUAGE DeriveGeneric #-}
module MiniJuvix.Syntax.Scoped.Name where

import MiniJuvix.Utils.Prelude
import qualified MiniJuvix.Syntax.Concrete.Name as C
import qualified Data.Stream
import Data.Stream (Stream (Cons))

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

data Name' n = Name' {
  nameId ∷ NameId
  , nameConcrete ∷ n
  , nameKind :: NameKind
  }
  deriving stock (Show)

instance Eq (Name' n) where
  (==) = (==) `on` nameId

instance Ord (Name' n) where
  compare = compare `on` nameId

instance Hashable (Name' n) where
  hashWithSalt salt = hashWithSalt salt . nameId

data NameKind
  = KNameConstructor -- ^ Constructor name.
  | KNameInductive -- ^ Name introduced by the inductive keyword.
  | KNameFunName  -- ^ Name of a defined function.
  | KNameLocal -- ^ A locally bound name (patterns, arguments, etc.).
  deriving stock (Show)
