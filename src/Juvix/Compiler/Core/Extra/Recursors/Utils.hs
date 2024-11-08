module Juvix.Compiler.Core.Extra.Recursors.Utils where

import Juvix.Compiler.Core.Extra.Recursors.Map.Named

shiftVar :: Index -> Var -> Var
shiftVar m v =
  let v' = over varIndex (+ m) v
   in if
          | v' ^. varIndex < 0 -> error "shifting a variable resulted in a negative index"
          | otherwise -> v'

-- | increase all free variable indices by a given value
shift :: Index -> Node -> Node
shift = \case
  0 -> id
  m -> umapN go
    where
      go k = \case
        NVar v
          | v ^. varIndex >= k -> NVar (shiftVar m v)
        n -> n
