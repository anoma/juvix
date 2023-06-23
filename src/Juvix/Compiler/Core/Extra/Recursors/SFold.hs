-- | Shallow fold recursors over 'Node'.
module Juvix.Compiler.Core.Extra.Recursors.SFold where

import Juvix.Compiler.Core.Extra.Base
import Juvix.Compiler.Core.Extra.Recursors.Base

sfoldG ::
  forall a f.
  (Applicative f) =>
  (a -> [a] -> a) ->
  (Node -> f a) ->
  Node ->
  f a
sfoldG uplus f = go
  where
    go :: Node -> f a
    go n = do
      mas' <- sequenceA mas
      n' <- f n
      pure (uplus n' mas')
      where
        mas :: [f a]
        mas = map go (schildren n)
