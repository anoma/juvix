module Juvix.Prelude.Base
  ( module Juvix.Prelude.Base.Foundation,
    module Juvix.Prelude.Effects,
    module Juvix.Prelude.Base,
  )
where

import Juvix.Prelude.Base.Foundation
import Juvix.Prelude.Effects

groupSortOnWithM :: forall a b m. (Ord b, Monad m) => (a -> m b) -> [a] -> m [(NonEmpty a, b)]
groupSortOnWithM f l = do
  l' <- mapWithM f l
  return (run . execAccumList . runInputList (sortOn snd l') $ repeatOnInput go)
  where
    go :: forall r. (Members '[Input (a, b), Accum (NonEmpty a, b)] r) => (a, b) -> Sem r ()
    go (e, eb) = do
      es <- map fst <$> inputWhile @(a, b) ((== eb) . snd)
      accum (e :| es, eb)

groupSortOnWith :: forall a b. (Ord b) => (a -> b) -> [a] -> [(NonEmpty a, b)]
groupSortOnWith f = runIdentity . groupSortOnWithM (return . f)

groupSortOnM :: (Ord b, Monad m) => (a -> m b) -> [a] -> m [NonEmpty a]
groupSortOnM f = fmap (map fst) . groupSortOnWithM f

groupSortOn :: (Ord b) => (a -> b) -> [a] -> [NonEmpty a]
groupSortOn f = map fst . groupSortOnWith f

groupSortOn' :: (Ord b) => (a -> b) -> [a] -> [[a]]
groupSortOn' f = map toList . groupSortOn f

findRepeatedOnM :: forall a b m. (Ord b, Monad m) => (a -> m b) -> [a] -> m [((a, NonEmpty a), b)]
findRepeatedOnM f = fmap (mapMaybe rep) . groupSortOnWithM f
  where
    rep :: (NonEmpty a, b) -> Maybe ((a, NonEmpty a), b)
    rep = \case
      ((x :| y : zs), b) -> Just ((x, y :| zs), b)
      _ -> Nothing

findRepeatedOn :: forall a b. (Ord b) => (a -> b) -> [a] -> [((a, NonEmpty a), b)]
findRepeatedOn f = runIdentity . findRepeatedOnM (return . f)

-- | Returns the repeated elements
findRepeated :: forall a. (Ord a) => [a] -> [a]
findRepeated = map (head . snd . fst) . findRepeatedOn id

allDifferent :: forall a. (Ord a) => [a] -> Bool
allDifferent = null . findRepeated

-- Function to group elements in fixed-size chunks
groupEvery :: Int -> [a] -> [[a]]
groupEvery _ [] = []
groupEvery n xs = xs' : groupEvery n rest
  where
    (xs', rest) = splitAt n xs

-- Format a number with dots every three digits
-- For example, 1234567890 becomes "1.234.567.890"
formatWithDots :: (Show a) => a -> String
formatWithDots n =
  let str = reverse (show n)
      groups = groupEvery 3 str
   in reverse (intercalate "." groups)
