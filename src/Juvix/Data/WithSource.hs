module Juvix.Data.WithSource where

import Juvix.Data.Fixity
import Juvix.Prelude.Base

data WithSource a = WithSource
  { _withSourceText :: Text,
    _withSourceValue :: a
  }
  deriving stock (Show, Data)

makeLenses ''WithSource

instance (HasAtomicity a) => HasAtomicity (WithSource a) where
  atomicity (WithSource _ a) = atomicity a

instance (Hashable a) => Hashable (WithSource a) where
  hashWithSalt a (WithSource _ p) = hashWithSalt a p

instance (Eq a) => Eq (WithSource a) where
  (==) = (==) `on` (^. withSourceValue)

instance (Ord a) => Ord (WithSource a) where
  compare = compare `on` (^. withSourceValue)

instance Functor WithSource where
  fmap = over withSourceValue

instance Foldable WithSource where
  foldMap f (WithSource _ a) = f a
  foldr f b (WithSource _ a) = f a b

instance Traversable WithSource where
  traverse f (WithSource i a) = WithSource i <$> f a
