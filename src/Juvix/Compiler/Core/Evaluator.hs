{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted extensions" #-}
{-# HLINT ignore "Avoid restricted flags" #-}

module Juvix.Compiler.Core.Evaluator where

import Control.Exception qualified as Exception
import Data.HashMap.Strict qualified as HashMap
import Debug.Trace qualified as Debug
import GHC.Conc qualified as GHC
import GHC.Show as S
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Error (CoreError (..))
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

-- | `eval ctx env n` evalues a node `n` whose all free variables point into
-- `env`. All nodes in `ctx` must be closed. All nodes in `env` must be values.
-- Invariant for values v: eval ctx env v = v
eval :: IdentContext -> Env -> Node -> Node
eval !ctx !env0 = convertRuntimeNodes . eval' env0
  where
    evalError :: Text -> Node -> a
    evalError msg node = Exception.throw (EvalError msg (Just node))

    eval' :: Env -> Node -> Node
    eval' !env !n = case n of
      NVar (Var _ idx) -> env !! idx
      NIdt (Ident _ sym) -> eval' [] (lookupContext n sym)
      NCst {} -> n
      NApp (App i l r) ->
        case eval' env l of
          Closure env' (Lambda _ b) -> let !v = eval' env r in eval' (v : env') b
          v -> evalError "invalid application" (mkApp i v (substEnv env r))
      NBlt (BuiltinApp _ op args) -> applyBuiltin n env op args
      NCtr (Constr i tag args) -> mkConstr i tag (map' (eval' env) args)
      NLam l@Lambda {} -> Closure env l
      NLet (Let _ v b) -> let !v' = eval' env v in eval' (v' : env) b
      NRec (LetRec _ vs b) ->
        let !vs' = map (eval' env') (toList vs)
            !env' = revAppend vs' env
         in foldr GHC.pseq (eval' env' b) vs'
      NCase (Case i v bs def) ->
        case eval' env v of
          NCtr (Constr _ tag args) -> branch n env args tag def bs
          v' -> evalError "matching on non-data" (substEnv env (mkCase i v' bs def))
      NPi {} -> substEnv env n
      NUniv {} -> n
      NTyp (TypeConstr i sym args) -> mkTypeConstr i sym (map' (eval' env) args)
      NDyn {} -> n
      Closure {} -> n

    branch :: Node -> Env -> [Node] -> Tag -> Maybe Node -> [CaseBranch] -> Node
    branch n !env !args !tag !def = \case
      (CaseBranch tag' _ b) : _
        | tag' == tag ->
            let !env' = revAppend args env
             in eval' env' b
      _ : bs' -> branch n env args tag def bs'
      [] -> case def of
        Just b -> eval' env b
        Nothing -> evalError "no matching case branch" (substEnv env n)

    applyBuiltin :: Node -> Env -> BuiltinOp -> [Node] -> Node
    applyBuiltin _ env OpIntAdd [l, r] = nodeFromInteger (integerFromNode (eval' env l) + integerFromNode (eval' env r))
    applyBuiltin _ env OpIntSub [l, r] = nodeFromInteger (integerFromNode (eval' env l) - integerFromNode (eval' env r))
    applyBuiltin _ env OpIntMul [l, r] = nodeFromInteger (integerFromNode (eval' env l) * integerFromNode (eval' env r))
    applyBuiltin n env OpIntDiv [l, r] =
      let !vl = eval' env l
       in case integerFromNode (eval' env r) of
            0 -> evalError "division by zero" (substEnv env n)
            k -> nodeFromInteger (div (integerFromNode vl) k)
    applyBuiltin n env OpIntMod [l, r] =
      let !vl = eval' env l
       in case integerFromNode (eval' env r) of
            0 -> evalError "division by zero" (substEnv env n)
            k -> nodeFromInteger (mod (integerFromNode vl) k)
    applyBuiltin _ env OpIntLt [l, r] = nodeFromBool (integerFromNode (eval' env l) < integerFromNode (eval' env r))
    applyBuiltin _ env OpIntLe [l, r] = nodeFromBool (integerFromNode (eval' env l) <= integerFromNode (eval' env r))
    applyBuiltin _ env OpEq [l, r] = nodeFromBool (structEq (eval' env l) (eval' env r))
    applyBuiltin _ env OpTrace [msg, x] = Debug.trace (printNode (eval' env msg)) (eval' env x)
    applyBuiltin _ env OpFail [msg] =
      Exception.throw (EvalError (fromString ("failure: " ++ printNode (eval' env msg))) Nothing)
    applyBuiltin n env _ _ = evalError "invalid builtin application" (substEnv env n)

    nodeFromInteger :: Integer -> Node
    nodeFromInteger !int = mkConstant' (ConstInteger int)

    nodeFromBool :: Bool -> Node
    nodeFromBool True = mkConstr' (BuiltinTag TagTrue) []
    nodeFromBool False = mkConstr' (BuiltinTag TagFalse) []

    integerFromNode :: Node -> Integer
    integerFromNode = \case
      NCst (Constant _ (ConstInteger int)) -> int
      v -> evalError "not an integer" v

    printNode :: Node -> String
    printNode = \case
      NCst (Constant _ (ConstString s)) -> fromText s
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
        NCtr (Constr _ (BuiltinTag TagReturn) [x]) ->
          return x
        NCtr (Constr _ (BuiltinTag TagBind) [x, f]) -> do
          x' <- hEvalIO hin hout ctx env x
          hEvalIO hin hout ctx env (mkApp Info.empty f x')
        NCtr (Constr _ (BuiltinTag TagWrite) [NCst (Constant _ (ConstString s))]) -> do
          hPutStr hout s
          return unitNode
        NCtr (Constr _ (BuiltinTag TagWrite) [arg]) -> do
          hPutStr hout (ppPrint arg)
          return unitNode
        NCtr (Constr _ (BuiltinTag TagReadLn) []) -> do
          hFlush hout
          mkConstant Info.empty . ConstString <$> hGetLine hin
        _ ->
          return node'
  where
    unitNode = mkConstr (Info.singleton (NoDisplayInfo ())) (BuiltinTag TagTrue) []

evalIO :: IdentContext -> Env -> Node -> IO Node
evalIO = hEvalIO stdin stdout

-- | Catch EvalError and convert it to CoreError. Needs a default location in case
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
