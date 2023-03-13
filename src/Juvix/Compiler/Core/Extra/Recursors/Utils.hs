module Juvix.Compiler.Core.Extra.Recursors.Utils where

import Juvix.Compiler.Core.Extra.Recursors.Map.Named
import Juvix.Compiler.Core.Language

shiftVar :: Index -> Var -> Var
shiftVar m = over varIndex (+ m)

-- | increase all free variable indices by a given value
shift :: Index -> Node -> Node
shift 0 = id
shift m = umapN go
  where
    go k = \case
      NVar v
        | v ^. varIndex >= k -> NVar (shiftVar m v)
      n -> n
