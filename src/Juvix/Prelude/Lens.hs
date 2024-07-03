module Juvix.Prelude.Lens where

import Juvix.Prelude.Base

-- | Points to the first element of a non-empty list.
_head1 :: Lens' (NonEmpty a) a
_head1 = singular each

_tail1 :: Lens' (NonEmpty a) [a]
_tail1 f (h :| hs) = do
  hs' <- f hs
  pure (h :| hs')

-- | View a non-empty list as the init part plus the last element.
_unsnoc1 :: Lens (NonEmpty a) (NonEmpty b) ([a], a) ([b], b)
_unsnoc1 afb la = uncurryF (|:) (afb (maybe [] toList minit, lasta))
  where
    (minit, lasta) = nonEmptyUnsnoc la

-- | Points to the last element of a non-empty list.
_last1 :: Lens' (NonEmpty a) a
_last1 = _unsnoc1 . _2

_self :: Prism' a a
_self = prism' id Just

overM :: (Applicative m) => Lens' a b -> (b -> m b) -> a -> m a
overM l f a = do
  a' <- f (a ^. l)
  return $ set l a' a

setAndRemember :: LensLike ((,) a) s t a b -> b -> s -> (a, t)
setAndRemember = (<<.~)

-- | Extracts the getter from a prism
prismView :: Prism s t a b -> b -> t
prismView aprism = withPrism aprism const

matchingMaybe :: Prism s s a b -> s -> Maybe a
matchingMaybe pri = either (const Nothing) return . matching pri
