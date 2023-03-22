{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted extensions" #-}
{-# HLINT ignore "Avoid restricted flags" #-}

module Juvix.Compiler.Core.Evaluator where

import Control.Exception qualified as Exception
import Data.HashMap.Strict qualified as HashMap
import GHC.Conc qualified as GHC
import GHC.IO (unsafePerformIO)
import GHC.Show qualified as S
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Error (CoreError (..))
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.NoDisplayInfo
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Pretty
import Text.Read qualified as T

data EvalError = EvalError
  { _evalErrorMsg :: !Text,
    _evalErrorNode :: !(Maybe Node)
  }

makeLenses ''EvalError

instance Show EvalError where
  show :: EvalError -> String
  show EvalError {..} =
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
eval :: Handle -> IdentContext -> Env -> Node -> Node
eval herr ctx env0 = convertRuntimeNodes . eval' env0
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
          Closure env' (Lambda _ _ b) -> let !v = eval' env r in eval' (v : env') b
          v -> evalError "invalid application" (mkApp i v (substEnv env r))
      NBlt (BuiltinApp _ op args) -> applyBuiltin n env op args
      NCtr (Constr i tag args) -> mkConstr i tag (map' (eval' env) args)
      NLam l@Lambda {} -> Closure env l
      NLet (Let _ (LetItem _ v) b) -> let !v' = eval' env v in eval' (v' : env) b
      NRec (LetRec _ vs b) ->
        let !vs' = map (eval' env' . (^. letItemValue)) (toList vs)
            !env' = revAppend vs' env
         in foldr GHC.pseq (eval' env' b) vs'
      NCase (Case i sym v bs def) ->
        case eval' env v of
          NCtr (Constr _ tag args) -> branch n env args tag def bs
          v' -> evalError "matching on non-data" (substEnv env (mkCase i sym v' bs def))
      NMatch (Match _ _ _ vs bs) ->
        let !vs' = map' (eval' env) (toList vs)
         in match n env vs' bs
      NPi {} -> substEnv env n
      NUniv {} -> n
      NTyp (TypeConstr i sym args) -> mkTypeConstr i sym (map' (eval' env) args)
      NPrim {} -> n
      NDyn {} -> n
      Closure {} -> n

    branch :: Node -> Env -> [Node] -> Tag -> Maybe Node -> [CaseBranch] -> Node
    branch n !env !args !tag !def = \case
      (CaseBranch _ tag' _ _ b) : _ | tag' == tag -> eval' (revAppend args env) b
      _ : bs' -> branch n env args tag def bs'
      [] -> case def of
        Just b -> eval' env b
        Nothing -> evalError "no matching case branch" (substEnv env n)

    match :: Node -> Env -> [Node] -> [MatchBranch] -> Node
    match n env vs = \case
      br : brs ->
        case matchPatterns [] vs (toList (br ^. matchBranchPatterns)) of
          Just args -> eval' (args ++ env) (br ^. matchBranchBody)
          Nothing -> match n env vs brs
        where
          matchPatterns :: [Node] -> [Node] -> [Pattern] -> Maybe [Node]
          matchPatterns acc (v : vs') (p : ps') =
            case patmatch acc v p of
              Just acc' -> matchPatterns acc' vs' ps'
              Nothing -> Nothing
          matchPatterns acc [] [] =
            Just acc
          matchPatterns _ _ _ =
            evalError "the number of patterns doesn't match the number of arguments" (substEnv env n)

          patmatch :: [Node] -> Node -> Pattern -> Maybe [Node]
          patmatch acc _ PatWildcard {} =
            Just acc
          patmatch acc v (PatBinder PatternBinder {..}) =
            patmatch (v : acc) v _patternBinderPattern
          patmatch acc (NCtr (Constr _ tag args)) (PatConstr PatternConstr {..})
            | tag == _patternConstrTag =
                matchPatterns acc args _patternConstrArgs
          patmatch _ _ _ = Nothing
      [] ->
        evalError "no matching pattern" (substEnv env n)

    applyBuiltin :: Node -> Env -> BuiltinOp -> [Node] -> Node
    applyBuiltin n env = \case
      OpIntAdd -> binNumOp (+)
      OpIntSub -> binNumOp (-)
      OpIntMul -> binNumOp (*)
      OpIntLt -> binNumCmpOp (<)
      OpIntLe -> binNumCmpOp (<=)
      OpEq -> binOp nodeFromBool id structEq
      OpIntDiv -> divOp quot
      OpIntMod -> divOp rem
      OpShow -> unary $ \arg -> mkConstant' (ConstString (ppPrint (eval' env arg)))
      OpStrConcat -> binary $ \arg1 arg2 ->
        case (eval' env arg1, eval' env arg2) of
          (NCst (Constant _ (ConstString s1)), NCst (Constant _ (ConstString s2))) ->
            mkConstant' (ConstString (s1 <> s2))
          _ ->
            evalError "string concatenation: argument not a string" n
      OpStrToInt -> unary $ \arg ->
        case eval' env arg of
          NCst (Constant _ (ConstString s)) ->
            case T.readMaybe (fromText s) of
              Just i ->
                mkConstant' (ConstInteger i)
              Nothing ->
                evalError "string to integer: not an integer" n
          _ ->
            evalError "string conversion: argument not a string" n
      OpFail -> unary $ \msg -> Exception.throw (EvalError ("failure: " <> printNode (eval' env msg)) Nothing)
      OpTrace -> binary $ \msg x -> unsafePerformIO (hPutStrLn herr (printNode (eval' env msg)) >> return (eval' env x))
      where
        err :: Text -> a
        err msg = evalError msg n

        unary :: (Node -> Node) -> [Node] -> Node
        unary op = \case
          [arg] -> op arg
          _ -> err "wrong number of arguments for unary operator"
        {-# INLINE unary #-}

        binary :: (Node -> Node -> Node) -> [Node] -> Node
        binary op = \case
          [l, r] -> op l r
          _ -> err "wrong number of arguments for binary operator"
        {-# INLINE binary #-}

        divOp :: (Integer -> Integer -> Integer) -> [Node] -> Node
        divOp op = binary $ \l r ->
          let !vl = eval' env l
           in case integerFromNode (eval' env r) of
                0 -> evalError "division by zero" (substEnv env n)
                k -> nodeFromInteger (op (integerFromNode vl) k)
        {-# INLINE divOp #-}

        binOp :: (b -> Node) -> (Node -> a) -> (a -> a -> b) -> [Node] -> Node
        binOp toNode toA op = binary $ \l r -> toNode (toA (eval' env l) `op` toA (eval' env r))
        {-# INLINE binOp #-}

        binNumCmpOp :: (Integer -> Integer -> Bool) -> [Node] -> Node
        binNumCmpOp = binOp nodeFromBool integerFromNode
        {-# INLINE binNumCmpOp #-}

        binNumOp :: (Integer -> Integer -> Integer) -> [Node] -> Node
        binNumOp = binOp nodeFromInteger integerFromNode
        {-# INLINE binNumOp #-}
    {-# INLINE applyBuiltin #-}

    nodeFromInteger :: Integer -> Node
    nodeFromInteger !int = mkConstant' (ConstInteger int)
    {-# INLINE nodeFromInteger #-}

    nodeFromBool :: Bool -> Node
    nodeFromBool b = mkConstr' (BuiltinTag tag) []
      where
        !tag
          | b = TagTrue
          | otherwise = TagFalse
    {-# INLINE nodeFromBool #-}

    integerFromNode :: Node -> Integer
    integerFromNode = \case
      NCst (Constant _ (ConstInteger int)) -> int
      v -> evalError "not an integer" v
    {-# INLINE integerFromNode #-}

    printNode :: Node -> Text
    printNode = \case
      NCst (Constant _ (ConstString s)) -> s
      v -> ppPrint v

    lookupContext :: Node -> Symbol -> Node
    lookupContext n sym =
      case HashMap.lookup sym ctx of
        Just n' -> n'
        Nothing -> evalError "symbol not defined" n
    {-# INLINE lookupContext #-}

-- Evaluate `node` and interpret the builtin IO actions.
hEvalIO :: Handle -> Handle -> Handle -> IdentContext -> Env -> Node -> IO Node
hEvalIO herr hin hout ctx env node =
  let node' = eval herr ctx env node
   in case node' of
        NCtr (Constr _ (BuiltinTag TagReturn) [x]) ->
          return x
        NCtr (Constr _ (BuiltinTag TagBind) [x, f]) -> do
          x' <- hEvalIO herr hin hout ctx env x
          hEvalIO herr hin hout ctx env (mkApp Info.empty f x')
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
evalIO = hEvalIO stderr stdin stdout

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
    { _coreErrorMsg = "evaluation error: " <> _evalErrorMsg,
      _coreErrorNode = _evalErrorNode,
      _coreErrorLoc = fromMaybe loc (lookupLocation =<< _evalErrorNode)
    }
