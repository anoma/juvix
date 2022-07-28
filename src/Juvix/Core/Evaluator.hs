module Juvix.Core.Evaluator where

import Data.HashMap.Strict ((!))
import Juvix.Core.Context
import Juvix.Core.Info qualified as Info
import Juvix.Core.Node
import Juvix.Core.Prelude

-- `eval ctx env n` evalues a node `n` whose all free variables point into
-- `env`. All nodes in `ctx` and `env` are closed.
eval :: IdentContext -> Env -> Node -> Node
eval !ctx = eval'
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
      Hole _ -> n
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
      Case _ v bs ->
        case eval' env v of
          Data _ tag args -> branch (args ++ env) tag bs
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
      Hole {} -> Suspended Info.empty (mkApp' n (map (eval' env) args))
      Axiom {} -> Suspended Info.empty (mkApp' n (map (eval' env) args))
      _ -> push env env' (eval' env' n) a args

    push' :: Env -> Env -> Node -> [Node] -> Node
    push' !env !env' !n !args = case args of
      a : args' -> push env env' n a args'
      [] -> eval' env' n

    branch :: Env -> Tag -> [CaseBranch] -> Node
    branch !env !tag = \case
      (CaseBranch tag' _ b) : _ | tag' == tag -> eval' env b
      _ : bs' -> branch env tag bs'
      [] -> evalError
