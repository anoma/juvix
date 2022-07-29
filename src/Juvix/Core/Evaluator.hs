module Juvix.Core.Evaluator where

import Data.HashMap.Strict ((!))
import Juvix.Core.Context
import Juvix.Core.Extra
import Juvix.Core.Language
import Juvix.Core.Language.Info qualified as Info

-- `eval ctx env n` evalues a node `n` whose all free variables point into
-- `env`. All nodes in `ctx` and `env` are closed and already evaluated.
eval :: IdentContext -> Env -> Node -> Node
eval !ctx !env0 = convertRuntimeNodes . eval' env0
  where
    unimplemented :: a
    unimplemented = error "not yet implemented"

    evalError :: a
    evalError = error "evaluation error"

    mkBuiltinClosure :: Env -> BuiltinOp -> Node
    mkBuiltinClosure = unimplemented

    mkConstructorClosure :: Env -> Tag -> Node
    mkConstructorClosure = unimplemented

    eval' :: Env -> Node -> Node
    eval' !env !n = case n of
      Var _ idx -> env !! idx
      Ident _ sym -> ctx ! sym
      Builtin _ op -> mkBuiltinClosure env op
      Constructor _ tag -> mkConstructorClosure env tag
      ConstValue _ _ -> n
      Axiom _ -> n
      App _ l r ->
        -- The semantics for evaluating application (App l r) is:
        --
        -- eval env (App l r) =
        --   case eval env l of
        --     LambdaClosure env' b -> eval (eval env r : env') b
        --
        -- To do this more efficently for builtins, constructors and
        -- multi-argument functions (without creating closures for each
        -- intermediate function and matching each twice) we gather all
        -- application arguments, evaluate them from left to right and
        -- push the results onto the environment.
        apply env l r []
      Lambda i b -> LambdaClosure i env b
      LetIn _ v b -> let !v' = eval' env v in eval' (v' : env) b
      Case _ v bs def ->
        case eval' env v of
          Data _ tag args -> branch env (args ++ env) tag def bs
          _ -> evalError
      If _ v b1 b2 ->
        case eval' env v of
          ConstValue _ (ConstBool True) -> eval' env b1
          ConstValue _ (ConstBool False) -> eval' env b2
          _ -> evalError
      Data {} -> n
      LambdaClosure {} -> n
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
      LambdaClosure _ env'' b -> push' env (eval' env a : env'') b args
      Constructor {} -> unimplemented
      Builtin {} -> unimplemented
      ConstValue {} -> evalError
      Data {} -> evalError
      Axiom {} -> Suspended Info.empty (mkApp' n (map (eval' env) args))
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
        Nothing -> evalError
