-- | The functions from this module are used in the pretty
-- printer. So it is convenient to have them isolated so that when debugging other
-- functions in Core.Utils we can use `ppTrace`
module Juvix.Compiler.Core.Extra.Utils.Base where

import Juvix.Compiler.Core.Extra.Base
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

freeVars :: SimpleFold Node Var
freeVars f = ufoldNA reassemble go
  where
    go k = \case
      NVar var@Var {..}
        | _varIndex >= k -> NVar <$> f (shiftVar (-k) var)
      n -> pure n

shiftVar :: Index -> Var -> Var
shiftVar m = over varIndex (+ m)

freeVarOccurrences :: Index -> SimpleFold Node Var
freeVarOccurrences idx = freeVars . filtered ((== idx) . (^. varIndex))

countFreeVarOccurrences :: Index -> Node -> Int
countFreeVarOccurrences idx n = length (n ^.. freeVarOccurrences idx)

varOccurs :: Index -> Node -> Bool
varOccurs idx = has (freeVarOccurrences idx)
