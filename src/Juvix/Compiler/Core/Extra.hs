module Juvix.Compiler.Core.Extra
  ( module Juvix.Compiler.Core.Extra,
    module Juvix.Compiler.Core.Extra.Base,
    module Juvix.Compiler.Core.Extra.Recursors,
    module Juvix.Compiler.Core.Extra.Info,
  )
where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Core.Extra.Base
import Juvix.Compiler.Core.Extra.Info
import Juvix.Compiler.Core.Extra.Recursors
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Language.Info qualified as Info

-- `isClosed` may short-circuit evaluation due to the use of `&&`, so it's not
-- entirely reducible to `getFreeVars` in terms of computation time.
isClosed :: Node -> Bool
isClosed = ufoldN (&&) go
  where
    go :: Index -> Node -> Bool
    go k = \case
      Var _ idx | idx >= k -> False
      _ -> True

getFreeVars :: Node -> HashSet Index
getFreeVars = gatherN go HashSet.empty
  where
    go :: Index -> HashSet Index -> Node -> HashSet Index
    go k acc = \case
      Var _ idx | idx >= k -> HashSet.insert (idx - k) acc
      _ -> acc

getIdents :: Node -> HashSet Symbol
getIdents = gather go HashSet.empty
  where
    go :: HashSet Symbol -> Node -> HashSet Symbol
    go acc = \case
      Ident _ sym -> HashSet.insert sym acc
      _ -> acc

countFreeVarOccurrences :: Index -> Node -> Int
countFreeVarOccurrences idx = gatherN go 0
  where
    go k acc = \case
      Var _ idx' | idx' == idx + k -> acc + 1
      _ -> acc

-- increase all free variable indices by a given value
shift :: Index -> Node -> Node
shift 0 = id
shift m = umapN go
  where
    go k n = case n of
      Var i idx | idx >= k -> Var i (idx + m)
      _ -> n

-- substitute a term t for the free variable with de Bruijn index 0, avoiding
-- variable capture
subst :: Node -> Node -> Node
subst t = umapN go
  where
    go k n = case n of
      Var _ idx | idx == k -> shift k t
      _ -> n

-- reduce all beta redexes present in a term and the ones created immediately
-- downwards (i.e., a "beta-development")
developBeta :: Node -> Node
developBeta = umap go
  where
    go :: Node -> Node
    go n = case n of
      App _ (Lambda _ body) arg -> subst arg body
      _ -> n

etaExpand :: Int -> Node -> Node
etaExpand 0 n = n
etaExpand k n = mkLambdas k (mkApp (shift k n) (map (Var Info.empty) (reverse [0 .. k - 1])))

-- substitution of all free variables for values in an environment
substEnv :: Env -> Node -> Node
substEnv env = if null env then id else umapN go
  where
    go k n = case n of
      Var _ idx | idx >= k -> env !! (idx - k)
      _ -> n

convertClosures :: Node -> Node
convertClosures = umap go
  where
    go :: Node -> Node
    go n = case n of
      Closure i env b -> substEnv env (Lambda i b)
      _ -> n

convertRuntimeNodes :: Node -> Node
convertRuntimeNodes = convertClosures
