-- | Fold recursors over 'Node'.
module Juvix.Compiler.Core.Extra.Recursors.Fold where

import Juvix.Compiler.Core.Extra.Base
import Juvix.Compiler.Core.Extra.Recursors.Base

ufoldG ::
  forall c a f.
  Applicative f =>
  Collector (Int, [Binder]) c ->
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
          map
            (\n' -> go ((coll ^. cCollect) (n' ^. childBindersNum, n' ^. childBinders) c) (n' ^. childNode))
            (ni ^. nodeChildren)
