{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted extensions" #-}
{-# HLINT ignore "Avoid restricted flags" #-}

module Juvix.Compiler.Core.Evaluator where

import Control.Exception qualified as Exception
import Data.HashMap.Strict qualified as HashMap
import GHC.Show
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Error
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Language.Info qualified as Info
import Juvix.Compiler.Core.Pretty

data EvalError = EvalError
  { _evalErrorMsg :: Text,
    _evalErrorNode :: Node
  }

makeLenses ''EvalError

instance Show EvalError where
  show :: EvalError -> String
  show (EvalError {..}) =
    "evaluation error: "
      ++ fromText _evalErrorMsg
      ++ ": "
      ++ fromText (ppTrace _evalErrorNode)

-- We definitely do _not_ want to wrap the evaluator in an exception monad / the
-- polysemy effects! This would almost double the execution time (whether an
-- error occurred needs to be checked at every point). Evaluation errors should
-- not happen for well-typed input (except perhaps division by zero), so it is
-- reasonable to catch them only at the CLI toplevel and just exit when they
-- occur. Use `catchEvalError` to catch evaluation errors in the IO monad.

instance Exception.Exception EvalError

-- `eval ctx env n` evalues a node `n` whose all free variables point into
-- `env`. All nodes in `ctx` must be closed. All nodes in `env` must be values.
-- Invariant for values v: eval ctx env v = v
eval :: IdentContext -> Env -> Node -> Node
eval !ctx !env0 = convertRuntimeNodes . eval' env0
  where
    evalError :: Text -> Node -> a
    evalError msg node = Exception.throw (EvalError msg node)

    eval' :: Env -> Node -> Node
    eval' !env !n = case n of
      Var _ idx -> env !! idx
      Ident _ sym -> eval' [] (lookupContext n sym)
      Constant {} -> n
      Axiom {} -> n
      App _ l r ->
        case eval' env l of
          Closure _ env' b -> let !v = eval' env r in eval' (v : env') b
          a@(Axiom {}) -> Suspended Info.empty (App Info.empty a (eval' env r))
          Suspended i t -> Suspended i (App Info.empty t (eval' env r))
          v -> evalError "invalid application" v
      BuiltinApp _ op args -> applyBuiltin n env op args
      ConstrApp i tag args -> Data i tag (map (eval' env) args)
      Lambda i b -> Closure i env b
      Let _ v b -> let !v' = eval' env v in eval' (v' : env) b
      Case _ v bs def ->
        case eval' env v of
          Data _ tag args -> branch n env (args ++ env) tag def bs
          v' -> evalError "matching on non-data" v'
      If _ v b1 b2 ->
        case eval' env v of
          Constant _ (ConstBool True) -> eval' env b1
          Constant _ (ConstBool False) -> eval' env b2
          v' -> evalError "conditional branch on a non-boolean" v'
      Data {} -> n
      Closure {} -> n
      Suspended {} -> n

    branch :: Node -> Env -> Env -> Tag -> Maybe Node -> [CaseBranch] -> Node
    branch n !denv !env !tag !def = \case
      (CaseBranch tag' _ b) : _ | tag' == tag -> eval' env b
      _ : bs' -> branch n denv env tag def bs'
      [] -> case def of
        Just b -> eval' denv b
        Nothing -> evalError "no matching case branch" (substEnv denv n)

    applyBuiltin :: Node -> Env -> BuiltinOp -> [Node] -> Node
    applyBuiltin _ env OpIntAdd [l, r] = nodeFromInteger (integerFromNode (eval' env l) + integerFromNode (eval' env r))
    applyBuiltin _ env OpIntSub [l, r] = nodeFromInteger (integerFromNode (eval' env l) - integerFromNode (eval' env r))
    applyBuiltin _ env OpIntMul [l, r] = nodeFromInteger (integerFromNode (eval' env l) * integerFromNode (eval' env r))
    applyBuiltin _ env OpIntDiv [l, r] = nodeFromInteger (div (integerFromNode (eval' env l)) (integerFromNode (eval' env r)))
    applyBuiltin _ env OpIntEq [l, r] = nodeFromBool (integerFromNode (eval' env l) == integerFromNode (eval' env r))
    applyBuiltin _ env OpIntLt [l, r] = nodeFromBool (integerFromNode (eval' env l) < integerFromNode (eval' env r))
    applyBuiltin _ env OpIntLe [l, r] = nodeFromBool (integerFromNode (eval' env l) <= integerFromNode (eval' env r))
    applyBuiltin n env _ _ = evalError "invalid builtin application" (substEnv env n)

    nodeFromInteger :: Integer -> Node
    nodeFromInteger !int = Constant Info.empty (ConstInteger int)

    nodeFromBool :: Bool -> Node
    nodeFromBool !b = Constant Info.empty (ConstBool b)

    integerFromNode :: Node -> Integer
    integerFromNode = \case
      Constant _ (ConstInteger int) -> int
      v -> evalError "not an integer" v

    lookupContext :: Node -> Symbol -> Node
    lookupContext n sym =
      case HashMap.lookup sym ctx of
        Just n' -> n'
        Nothing -> Suspended Info.empty n

-- Catch EvalError and convert it to CoreError. Needs a default location in case
-- no location is available in EvalError.
catchEvalError :: Location -> a -> IO (Either CoreError a)
catchEvalError loc a = do
  Exception.catch
    (return (Right a))
    (\(ex :: EvalError) -> return (Left (toCoreError ex)))
  where
    toCoreError :: EvalError -> CoreError
    toCoreError (EvalError {..}) =
      CoreError
        { _coreErrorMsg = _evalErrorMsg,
          _coreErrorNode = Just _evalErrorNode,
          _coreErrorLoc = fromMaybe loc (lookupLocation _evalErrorNode)
        }
