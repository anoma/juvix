{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted extensions" #-}
{-# HLINT ignore "Avoid restricted flags" #-}

module Juvix.Compiler.Core.Evaluator where

import Control.Exception qualified as Exception
import Data.HashMap.Strict qualified as HashMap
import Debug.Trace qualified as Debug
import GHC.Show as S
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Error
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.NoDisplayInfo
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Pretty

data EvalError = EvalError
  { _evalErrorMsg :: !Text,
    _evalErrorNode :: !(Maybe Node)
  }

makeLenses ''EvalError

instance Show EvalError where
  show :: EvalError -> String
  show (EvalError {..}) =
    "evaluation error: "
      ++ fromText _evalErrorMsg
      ++ case _evalErrorNode of
        Nothing -> ""
        Just node -> ": " ++ fromText (ppTrace node)

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
    evalError !msg !node = Exception.throw (EvalError msg (Just node))

    eval' :: Env -> Node -> Node
    eval' !env !n = case n of
      Var _ idx -> env !! idx
      Ident _ sym -> eval' [] (lookupContext n sym)
      Constant {} -> n
      App i l r ->
        case eval' env l of
          Closure _ env' b -> let !v = eval' env r in eval' (v : env') b
          v -> evalError "invalid application" (App i v (substEnv env r))
      BuiltinApp _ op args -> applyBuiltin n env op args
      Constr i tag args -> Constr i tag (map (eval' env) args)
      Lambda i b -> Closure i env b
      Let _ v b -> let !v' = eval' env v in eval' (v' : env) b
      Case i v bs def ->
        case eval' env v of
          Constr _ tag args -> branch n env args tag def bs
          v' -> evalError "matching on non-data" (substEnv env (Case i v' bs def))
      If i v b1 b2 ->
        case eval' env v of
          Constant _ (ConstBool True) -> eval' env b1
          Constant _ (ConstBool False) -> eval' env b2
          v' -> evalError "conditional branch on a non-boolean" (substEnv env (If i v' b1 b2))
      Pi {} -> substEnv env n -- this might need to be implemented more efficiently later
      Univ {} -> n
      TypeApp i sym args -> TypeApp i sym (map (eval' env) args)
      Closure {} -> n

    branch :: Node -> Env -> [Node] -> Tag -> Maybe Node -> [CaseBranch] -> Node
    branch n !env !args !tag !def = \case
      (CaseBranch tag' _ b) : _ | tag' == tag -> eval' (revAppend args env) b
      _ : bs' -> branch n env args tag def bs'
      [] -> case def of
        Just b -> eval' env b
        Nothing -> evalError "no matching case branch" (substEnv env n)

    applyBuiltin :: Node -> Env -> BuiltinOp -> [Node] -> Node
    applyBuiltin _ env OpIntAdd [l, r] = nodeFromInteger (integerFromNode (eval' env l) + integerFromNode (eval' env r))
    applyBuiltin _ env OpIntSub [l, r] = nodeFromInteger (integerFromNode (eval' env l) - integerFromNode (eval' env r))
    applyBuiltin _ env OpIntMul [l, r] = nodeFromInteger (integerFromNode (eval' env l) * integerFromNode (eval' env r))
    applyBuiltin n env OpIntDiv [l, r] =
      case integerFromNode (eval' env r) of
        0 -> evalError "division by zero" (substEnv env n)
        k -> nodeFromInteger (div (integerFromNode (eval' env l)) k)
    applyBuiltin n env OpIntMod [l, r] =
      case integerFromNode (eval' env r) of
        0 -> evalError "division by zero" (substEnv env n)
        k -> nodeFromInteger (mod (integerFromNode (eval' env l)) k)
    applyBuiltin _ env OpIntLt [l, r] = nodeFromBool (integerFromNode (eval' env l) < integerFromNode (eval' env r))
    applyBuiltin _ env OpIntLe [l, r] = nodeFromBool (integerFromNode (eval' env l) <= integerFromNode (eval' env r))
    applyBuiltin _ env OpEq [l, r] = nodeFromBool (eval' env l == eval' env r)
    applyBuiltin _ env OpTrace [msg, x] = Debug.trace (printNode (eval' env msg)) (eval' env x)
    applyBuiltin _ env OpFail [msg] =
      Exception.throw (EvalError (fromString ("failure: " ++ printNode (eval' env msg))) Nothing)
    applyBuiltin n env _ _ = evalError "invalid builtin application" (substEnv env n)

    nodeFromInteger :: Integer -> Node
    nodeFromInteger !int = Constant Info.empty (ConstInteger int)

    nodeFromBool :: Bool -> Node
    nodeFromBool !b = Constant Info.empty (ConstBool b)

    integerFromNode :: Node -> Integer
    integerFromNode = \case
      Constant _ (ConstInteger int) -> int
      v -> evalError "not an integer" v

    printNode :: Node -> String
    printNode = \case
      Constant _ (ConstString s) -> fromText s
      v -> fromText $ ppPrint v

    lookupContext :: Node -> Symbol -> Node
    lookupContext n sym =
      case HashMap.lookup sym ctx of
        Just n' -> n'
        Nothing -> evalError "symbol not defined" n

-- Evaluate `node` and interpret the builtin IO actions.
hEvalIO :: Handle -> Handle -> IdentContext -> Env -> Node -> IO Node
hEvalIO hin hout ctx env node =
  let node' = eval ctx env node
   in case node' of
        Constr _ (BuiltinTag TagReturn) [x] ->
          return x
        Constr _ (BuiltinTag TagBind) [x, f] -> do
          x' <- hEvalIO hin hout ctx env x
          hEvalIO hin hout ctx env (App Info.empty f x')
        Constr _ (BuiltinTag TagWrite) [Constant _ (ConstString s)] -> do
          hPutStr hout s
          return unitNode
        Constr _ (BuiltinTag TagWrite) [arg] -> do
          hPutStr hout (ppPrint arg)
          return unitNode
        Constr _ (BuiltinTag TagReadLn) [] -> do
          hFlush hout
          Constant Info.empty . ConstString <$> hGetLine hin
        _ ->
          return node'
  where
    unitNode = Constr (Info.singleton (NoDisplayInfo ())) (BuiltinTag TagNil) []

evalIO :: IdentContext -> Env -> Node -> IO Node
evalIO = hEvalIO stdin stdout

-- Catch EvalError and convert it to CoreError. Needs a default location in case
-- no location is available in EvalError.
catchEvalError :: Location -> a -> IO (Either CoreError a)
catchEvalError loc a =
  Exception.catch
    (Exception.evaluate a <&> Right)
    (\(ex :: EvalError) -> return (Left (toCoreError loc ex)))

catchEvalErrorIO :: Location -> IO a -> IO (Either CoreError a)
catchEvalErrorIO loc ma =
  Exception.catch
    (Exception.evaluate ma >>= \ma' -> ma' <&> Right)
    (\(ex :: EvalError) -> return (Left (toCoreError loc ex)))

toCoreError :: Location -> EvalError -> CoreError
toCoreError loc (EvalError {..}) =
  CoreError
    { _coreErrorMsg = mappend "evaluation error: " _evalErrorMsg,
      _coreErrorNode = _evalErrorNode,
      _coreErrorLoc = fromMaybe loc (lookupLocation =<< _evalErrorNode)
    }
