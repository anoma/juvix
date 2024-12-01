module Juvix.Prelude.Base
  ( module Juvix.Prelude.Base.Foundation,
    module Juvix.Prelude.Effects,
    module Juvix.Prelude.Base,
  )
where

import Juvix.Prelude.Base.Foundation
import Juvix.Prelude.Effects

groupSortOnWith :: forall a b. (Ord b) => (a -> b) -> [a] -> [(NonEmpty a, b)]
groupSortOnWith f l = run . execAccumList . runInputList (sortOn snd (mapWith f l)) $ repeatOnInput go
  where
    go :: forall r. (Members '[Input (a, b), Accum (NonEmpty a, b)] r) => (a, b) -> Sem r ()
    go (e, eb) = do
      es <- map fst <$> inputWhile @(a, b) ((== eb) . snd)
      accum (e :| es, eb)

groupSortOn :: (Ord b) => (a -> b) -> [a] -> [NonEmpty a]
groupSortOn f = map fst . groupSortOnWith f

groupSortOn' :: (Ord b) => (a -> b) -> [a] -> [[a]]
groupSortOn' f = map toList . groupSortOn f

findRepeatedOn :: forall a b. (Ord b) => (a -> b) -> [a] -> [(NonEmpty a, b)]
findRepeatedOn f = mapMaybe rep . groupSortOnWith f
  where
    rep :: (NonEmpty a, b) -> Maybe (NonEmpty a, b)
    rep = \case
      (n@(_ :| _ : _), b) -> Just (n, b)
      _ -> Nothing

-- | Returns the repeated elements
findRepeated :: forall a. (Ord a) => [a] -> [a]
findRepeated = mapMaybe rep . groupSortOn id
  where
    rep :: NonEmpty a -> Maybe a
    rep = \case
      (a :| _ : _) -> Just a
      _ -> Nothing

allDifferent :: forall a. (Ord a) => [a] -> Bool
allDifferent = null . findRepeated
