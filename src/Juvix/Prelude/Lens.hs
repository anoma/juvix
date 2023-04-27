module Juvix.Prelude.Lens where

import Juvix.Prelude.Base

-- | points to the first element of a non-empty list.
_head1 :: Lens' (NonEmpty a) a
_head1 = singular each

overM :: Applicative m => Lens' a b -> (b -> m b) -> a -> m a
overM l f a = do
  a' <- f (a ^. l)
  return $ set l a' a
