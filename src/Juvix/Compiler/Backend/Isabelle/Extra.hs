module Juvix.Compiler.Backend.Isabelle.Extra where

import Data.List.NonEmpty.Extra qualified as NonEmpty
import Juvix.Compiler.Backend.Isabelle.Language

mkApp :: Expression -> [Expression] -> Expression
mkApp fn = \case
  [] -> fn
  arg : args -> mkApp (ExprApp (Application fn arg)) args

-- | Check if a pattern `pat1` subsumes another pattern `pat2`.
subsumesPattern :: Pattern -> Pattern -> Bool
subsumesPattern pat1 pat2 = case (pat1, pat2) of
  (PatVar _, _) -> True
  (PatZero, PatZero) -> True
  (PatConstrApp (ConstrApp c1 p1), PatConstrApp (ConstrApp c2 p2)) ->
    c1 == c2 && all (uncurry subsumesPattern) (zipExact p1 p2)
  (PatTuple (Tuple p1), PatTuple (Tuple p2)) ->
    length p1 == length p2
      && all (uncurry subsumesPattern) (NonEmpty.zip p1 p2)
  (PatList (List p1), PatList (List p2)) ->
    length p1 == length p2
      && all (uncurry subsumesPattern) (zipExact p1 p2)
  (PatCons (Cons c1 p1), PatCons (Cons c2 p2)) ->
    subsumesPattern c1 c2 && subsumesPattern p1 p2
  (PatRecord (Record n1 r1), PatRecord (Record n2 r2)) ->
    n1 == n2
      && map fst r1' == map fst r2'
      && all (uncurry subsumesPattern) (zipExact (map snd r1') (map snd r2'))
    where
      r1' = sortOn fst r1
      r2' = sortOn fst r2
  _ -> False
