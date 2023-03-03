-- | The functions from this module are used in the pretty
-- printer. So it is convenient to have them isolated so that when debugging other
-- functions in Core.Utils we can use `ppTrace`
module Juvix.Compiler.Core.Extra.Utils.Base where

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

countFreeVarOccurrences :: Index -> Node -> Int
countFreeVarOccurrences idx = gatherN go 0
  where
    go k acc = \case
      NVar (Var _ idx') | idx' == idx + k -> acc + 1
      _ -> acc

varOccurs :: Index -> Node -> Bool
varOccurs idx node = countFreeVarOccurrences idx node > 0
