{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted extensions" #-}
{-# HLINT ignore "Avoid restricted flags" #-}

module Juvix.Core.Evaluator where

import Control.Exception qualified as Exception
import Data.HashMap.Strict ((!))
import GHC.Show
import Juvix.Core.Extra
import Juvix.Core.Language
import Juvix.Core.Language.Info qualified as Info
import Juvix.Core.Data.InfoTable

newtype EvalError = EvalError String

instance Show EvalError where
  show :: EvalError -> String
  show (EvalError msg) = "evaluation error: " ++ msg

instance Exception.Exception EvalError

-- We definitely do _not_ want to wrap the evaluator in an exception monad / the
-- polysemy effects! This would almost double the execution time. Evaluation
-- errors should not happen for well-typed input (except perhaps division by
-- zero), so it is reasonable to catch them only at the CLI toplevel and just
-- exit when they occur.

-- `eval ctx env n` evalues a node `n` whose all free variables point into
-- `env`. All nodes in `ctx` must be closed. All nodes in `env` must be values.
-- Invariant for values v: eval ctx env v = v
eval :: IdentContext -> Env -> Node -> Node
eval !ctx !env0 = convertRuntimeNodes . eval' env0
  where
    evalError :: String -> a
    evalError msg = Exception.throw (EvalError msg)

    eval' :: Env -> Node -> Node
    eval' !env !n = case n of
      Var _ idx -> env !! idx
      Ident _ sym -> eval' [] (ctx ! sym)
      Constant {} -> n
      Axiom {} -> n
      App _ l r ->
        case eval' env l of
          Closure _ env' b -> let !v = eval' env r in eval' (v : env') b
          a@(Axiom {}) -> Suspended Info.empty (App Info.empty a (eval' env r))
          Suspended i t -> Suspended i (App Info.empty t (eval' env r))
          _ -> evalError "invalid application"
      BuiltinApp _ op args -> applyBuiltin env op args
      ConstrApp i tag args -> Data i tag (map (eval' env) args)
      Lambda i b -> Closure i env b
      Let _ v b -> let !v' = eval' env v in eval' (v' : env) b
      Case _ v bs def ->
        case eval' env v of
          Data _ tag args -> branch env (args ++ env) tag def bs
          _ -> evalError "matching on non-data"
      If _ v b1 b2 ->
        case eval' env v of
          Constant _ (ConstBool True) -> eval' env b1
          Constant _ (ConstBool False) -> eval' env b2
          _ -> evalError "conditional branch on a non-boolean"
      Data {} -> n
      Closure {} -> n
      Suspended {} -> n

    branch :: Env -> Env -> Tag -> Maybe Node -> [CaseBranch] -> Node
    branch !denv !env !tag !def = \case
      (CaseBranch tag' _ b) : _ | tag' == tag -> eval' env b
      _ : bs' -> branch denv env tag def bs'
      [] -> case def of
        Just b -> eval' denv b
        Nothing -> evalError "no matching case branch"

    applyBuiltin :: Env -> BuiltinOp -> [Node] -> Node
    applyBuiltin env OpIntAdd [l, r] = nodeFromInteger (integerFromNode (eval' env l) + integerFromNode (eval' env r))
    applyBuiltin env OpIntSub [l, r] = nodeFromInteger (integerFromNode (eval' env l) - integerFromNode (eval' env r))
    applyBuiltin env OpIntMul [l, r] = nodeFromInteger (integerFromNode (eval' env l) * integerFromNode (eval' env r))
    applyBuiltin env OpIntDiv [l, r] = nodeFromInteger (div (integerFromNode (eval' env l)) (integerFromNode (eval' env r)))
    applyBuiltin env OpIntEq [l, r] = nodeFromBool (integerFromNode (eval' env l) == integerFromNode (eval' env r))
    applyBuiltin env OpIntLt [l, r] = nodeFromBool (integerFromNode (eval' env l) < integerFromNode (eval' env r))
    applyBuiltin env OpIntLe [l, r] = nodeFromBool (integerFromNode (eval' env l) <= integerFromNode (eval' env r))
    applyBuiltin env OpBoolAnd [l, r] = nodeFromBool (boolFromNode (eval' env l) && boolFromNode (eval' env r))
    applyBuiltin env OpBoolOr [l, r] = nodeFromBool (boolFromNode (eval' env l) || boolFromNode (eval' env r))
    applyBuiltin _ _ _ = evalError "invalid builtin application"

    nodeFromInteger :: Integer -> Node
    nodeFromInteger !int = Constant Info.empty (ConstInteger int)

    nodeFromBool :: Bool -> Node
    nodeFromBool !b = Constant Info.empty (ConstBool b)

    integerFromNode :: Node -> Integer
    integerFromNode = \case
      Constant _ (ConstInteger int) -> int
      _ -> evalError "not an integer"

    boolFromNode :: Node -> Bool
    boolFromNode = \case
      Constant _ (ConstBool b) -> b
      _ -> evalError "not a boolean"
