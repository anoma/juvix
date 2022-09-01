module Juvix.Compiler.Core.Extra.Equality where

import Juvix.Compiler.Core.Language

-- | Structural equality on Nodes. Semantically the same as `==` but slightly
-- optimised. Used mostly in the evaluator.
structEq :: Node -> Node -> Bool
structEq (NCst (Constant _ v1)) (NCst (Constant _ v2)) = v1 == v2
structEq (NCtr (Constr _ tag1 args1)) (NCtr (Constr _ tag2 args2)) =
  tag1 == tag2 && argsEq args1 args2
  where
    argsEq :: [Node] -> [Node] -> Bool
    argsEq [] [] = True
    argsEq (x:xs) (y:ys) | structEq x y = argsEq xs ys
    argsEq _ _ = False
structEq x y = x == y
