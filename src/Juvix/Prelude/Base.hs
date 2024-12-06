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

findRepeatedOnM :: forall a b m. (Ord b, Monad m) => (a -> m b) -> [a] -> m [(NonEmpty a, b)]
findRepeatedOnM f = fmap (mapMaybe rep) . groupSortOnWithM f
  where
    rep :: (NonEmpty a, b) -> Maybe (NonEmpty a, b)
    rep = \case
      (n@(_ :| _ : _), b) -> Just (n, b)
      _ -> Nothing

findRepeatedOn :: forall a b. (Ord b) => (a -> b) -> [a] -> [(NonEmpty a, b)]
findRepeatedOn f = runIdentity . findRepeatedOnM (return . f)

-- | Returns the repeated elements
findRepeated :: forall a. (Ord a) => [a] -> [a]
findRepeated = map (head . fst) . findRepeatedOn id

allDifferent :: forall a. (Ord a) => [a] -> Bool
allDifferent = null . findRepeated
