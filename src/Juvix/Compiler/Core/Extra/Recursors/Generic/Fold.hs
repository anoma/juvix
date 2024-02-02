module Juvix.Compiler.Core.Extra.Recursors.Generic.Fold where

import Juvix.Compiler.Core.Extra.Recursors.Generic.Base

ufoldG ::
  forall c a f n d ch b.
  (IsNode n d ch b) =>
  (Applicative f) =>
  Collector (Int, [b]) c ->
  (a -> [a] -> a) ->
  (c -> n -> f a) ->
  n ->
  f a
ufoldG coll uplus f = go (coll ^. cEmpty)
  where
    go :: c -> n -> f a
    go c n = do
      mas' <- sequenceA mas
      n' <- f c n
      pure (uplus n' mas')
      where
        ni :: d
        ni = gDestruct n
        mas :: [f a]
        mas =
          map
            (\n' -> go ((coll ^. cCollect) (gBindersNum n', gBinders n') c) (gChild n'))
            (gChildren ni)
