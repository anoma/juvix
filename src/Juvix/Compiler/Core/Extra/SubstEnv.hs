-- | This module contains `substEnv`. This function is used in the pretty
-- printer. So it is convenient to have it isolated so that when debugging other
-- functions in Core.Utils we can use `ppTrace`
module Juvix.Compiler.Core.Extra.SubstEnv where

import Juvix.Compiler.Core.Extra.Recursors
import Juvix.Compiler.Core.Language

-- | substitution of all free variables for values in an environment
substEnv :: Env -> Node -> Node
substEnv env
  | null env = id
  | otherwise = umapN go
  where
    go k n = case n of
      NVar (Var _ idx)
        | idx >= k -> env !! (idx - k)
      _ -> n
