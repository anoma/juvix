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
import Juvix.Core.Types.InfoTable

newtype EvalError = EvalError String

instance Show EvalError where
  show :: EvalError -> String
  show (EvalError msg) = "evaluation error: " ++ msg

instance Exception.Exception EvalError

-- We definitely do *not* want to wrap the evaluator in an exception monad / the
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
        -- The semantics for evaluating application (App l r) is:
        --
        -- eval env (App l r) =
        --   case eval env l of
        --     Closure env' b ->
        --       let !v = eval env r in eval (v : env') b
        --
        -- To do this more efficently for multi-argument functions (without
        -- creating closures for each intermediate function and matching each
        -- twice) we gather all application arguments, evaluate them from left
        -- to right and push the results onto the environment.
        apply env l r []
      BuiltinApp _ op args -> applyBuiltin env op args
      ConstructorApp i tag args -> Data i tag (map (eval' env) args)
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

    apply :: Env -> Node -> Node -> [Node] -> Node
    apply !env !n !a !args = case n of
      App _ l r -> apply env l r (a : args)
      _ -> push env env n a args

    -- In `push env env' n args`, `a` is the first argument, `env` is the
    -- environment of `a` and `args`, `env'` the environment of `n`.
    push :: Env -> Env -> Node -> Node -> [Node] -> Node
    push !env !env' !n !a !args = case n of
      Lambda _ b -> push' env (eval' env a : env') b args
      Closure _ env'' b -> push' env (eval' env a : env'') b args
      Constant {} -> evalError "applying a constant to an argument"
      ConstructorApp {} -> evalError "constructor applied to too many arguments"
      BuiltinApp {} -> evalError "builtin applied to too many arguments"
      Data {} -> evalError "constructor applied to too many arguments"
      Axiom {} -> Suspended Info.empty (mkApp' n (map (eval' env) args))
      Suspended i t -> Suspended i (mkApp' t (map (eval' env) args))
      Ident _ sym -> push env env' (ctx ! sym) a args
      _ -> push env env' (eval' env' n) a args

    push' :: Env -> Env -> Node -> [Node] -> Node
    push' !env !env' !n !args = case args of
      a : args' -> push env env' n a args'
      [] -> eval' env' n

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
    applyBuiltin _ _ _ = evalError "unrecognised builtin application"

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
