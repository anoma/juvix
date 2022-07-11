module Juvix.Syntax.Loc where

import Juvix.Prelude
import Juvix.Prelude.Pretty
import Juvix.Syntax.Fixity

data WithLoc a = WithLoc
  { _withLocInt :: Interval,
    _withLocParam :: a
  }
  deriving stock (Show)

makeLenses ''WithLoc

instance HasLoc (WithLoc a) where
  getLoc = (^. withLocInt)

instance HasAtomicity a => HasAtomicity (WithLoc a) where
  atomicity (WithLoc _ a) = atomicity a

instance Hashable a => Hashable (WithLoc a) where
  hashWithSalt a (WithLoc _ p) = hashWithSalt a p

instance Eq a => Eq (WithLoc a) where
  (==) = (==) `on` (^. withLocParam)

instance Ord a => Ord (WithLoc a) where
  compare = compare `on` (^. withLocParam)

instance Functor WithLoc where
  fmap = over withLocParam

instance Pretty a => Pretty (WithLoc a) where
  pretty (WithLoc _ a) = pretty a
