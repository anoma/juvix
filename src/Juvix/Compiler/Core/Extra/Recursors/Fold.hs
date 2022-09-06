-- | Fold recursors over 'Node'.
module Juvix.Compiler.Core.Extra.Recursors.Fold where

import Juvix.Compiler.Core.Extra.Base
import Juvix.Compiler.Core.Extra.Recursors.Base

ufoldG ::
  forall c a f.
  Applicative f =>
  Collector (Int, [Info]) c ->
  (a -> [a] -> a) ->
  (c -> Node -> f a) ->
  Node ->
  f a
ufoldG coll uplus f = go (coll ^. cEmpty)
  where
    go :: c -> Node -> f a
    go c n = do
      mas' <- sequenceA mas
      n' <- f c n
      pure (uplus n' mas')
      where
        ni :: NodeDetails
        ni = destruct n
        mas :: [f a]
        mas =
          zipWith3Exact
            (\n' k bis -> go ((coll ^. cCollect) (k, bis) c) n')
            (ni ^. nodeChildren)
            (ni ^. nodeChildBindersNum)
            (ni ^. nodeChildBindersInfo)
