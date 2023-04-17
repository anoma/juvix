module Juvix.Data.Emacs.Point
  ( Point,
    fromZeroBasedInt,
    fromOneBasedInt,
    pointSuccN,
    WithRange (..),
    pintervalStart,
    pintervalEnd,
    withRange,
    withRangeParam,
  )
where

import Juvix.Prelude.Base
import Juvix.Prelude.Pretty

-- | Emacs point
newtype Point = Point
  { _unpoint :: Word64
  }
  deriving stock (Ord, Eq, Show, Data)

instance Enum Point where
  toEnum :: Int -> Point
  toEnum = fromOneBasedInt

  fromEnum :: Point -> Int
  fromEnum (Point n) = fromIntegral n

  succ :: Point -> Point
  succ (Point n) = Point (succ n)

pointSuccN :: Word64 -> Point -> Point
pointSuccN n (Point p) = Point (n + p)

fromOneBasedInt :: Int -> Point
fromOneBasedInt n
  | n <= 0 = error "must be positive"
  | otherwise = Point (fromIntegral n)

fromZeroBasedInt :: Int -> Point
fromZeroBasedInt n
  | n < 0 = error "must be non-negative"
  | otherwise = fromOneBasedInt (n + 1)

instance Bounded Point where
  minBound = Point 1
  maxBound = Point maxBound

data PointInterval = PointInterval
  { _pintervalStart :: Point,
    _pintervalEnd :: Point
  }
  deriving stock (Show)

data WithRange a = WithRange
  { _withRange :: PointInterval,
    _withRangeParam :: a
  }
  deriving stock (Show)

makeLenses ''PointInterval
makeLenses ''WithRange

instance Hashable a => Hashable (WithRange a) where
  hashWithSalt a (WithRange _ p) = hashWithSalt a p

instance Eq a => Eq (WithRange a) where
  (==) = (==) `on` (^. withRangeParam)

instance Ord a => Ord (WithRange a) where
  compare = compare `on` (^. withRangeParam)

instance Functor WithRange where
  fmap = over withRangeParam

instance Foldable WithRange where
  foldMap f (WithRange _ a) = f a
  foldr f b (WithRange _ a) = f a b

instance Traversable WithRange where
  traverse f (WithRange i a) = WithRange i <$> f a

instance Pretty a => Pretty (WithRange a) where
  pretty (WithRange _ a) = pretty a
