module Juvix.Compiler.Core.Evaluator where

import Control.Exception qualified as Exception
import Data.HashMap.Strict qualified as HashMap
import GHC.Base (seq)
import GHC.Conc qualified as GHC
import GHC.IO (unsafePerformIO)
import GHC.Show qualified as S
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Error (CoreError (..))
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.NoDisplayInfo
import Juvix.Compiler.Core.Pretty
import Juvix.Data.Field
import Text.Read qualified as T

data EvalOptions = EvalOptions
  { _evalOptionsNormalize :: Bool,
    _evalOptionsNoFailure :: Bool,
    _evalOptionsSilent :: Bool,
    _evalOptionsFieldSize :: Natural
  }

makeLenses ''EvalOptions

defaultEvalOptions :: EvalOptions
defaultEvalOptions =
  EvalOptions
    { _evalOptionsNormalize = False,
      _evalOptionsNoFailure = False,
      _evalOptionsSilent = False,
      _evalOptionsFieldSize = defaultFieldSize
    }

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

evalInfoTable :: Handle -> InfoTable -> Node
evalInfoTable herr info = eval herr idenCtxt [] mainNode
  where
    idenCtxt = info ^. identContext
    mainSym = fromJust (info ^. infoMain)
    mainNode = fromJust (HashMap.lookup mainSym idenCtxt)

-- | `eval ctx env n` evaluates a node `n` whose all free variables point into
-- `env`. All nodes in `ctx` must be closed. All nodes in `env` must be values.
-- Invariant for values v: eval ctx env v = v
eval :: Handle -> IdentContext -> Env -> Node -> Node
eval herr ctx env = geval defaultEvalOptions herr ctx env

geval :: EvalOptions -> Handle -> IdentContext -> Env -> Node -> Node
geval opts herr ctx env0 = eval' env0
  where
    evalError :: Text -> Node -> a
    evalError msg node = Exception.throw (EvalError msg (Just node))

    eval' :: Env -> Node -> Node
    eval' !env !n = case n of
      NVar (Var _ idx) -> env !! idx
      NIdt (Ident _ sym) -> lookupContext n sym
      NCst {} -> n
      NApp (App i l r) ->
        case eval' env l of
          Closure env' (NLam (Lambda i' bi b)) ->
            let !v = eval' env r in evalBody i' bi env' v b
          lv
            | opts ^. evalOptionsNormalize || opts ^. evalOptionsNoFailure ->
                let !v = eval' env r in goNormApp i lv v
            | otherwise ->
                evalError "invalid application" (mkApp i lv (substEnv env r))
      NBlt (BuiltinApp _ op args) -> applyBuiltin n env op args
      NCtr (Constr i tag args) -> mkConstr i tag (map' (eval' env) args)
      NLam {} -> Closure env n
      NLet (Let i (LetItem bi v) b) -> let !v' = eval' env v in evalBody i bi env v' b
      NRec (LetRec _ vs b) ->
        let !vs' = map (eval' env' . (^. letItemValue)) (toList vs)
            !env' = revAppend vs' env
         in foldr GHC.pseq (eval' env' b) vs'
      NCase (Case i sym v bs def) ->
        case eval' env v of
          NCtr (Constr _ tag args) ->
            branch n env args tag def bs
          v'
            | opts ^. evalOptionsNormalize || opts ^. evalOptionsNoFailure ->
                goNormCase env i sym v' bs def
            | otherwise ->
                evalError "matching on non-data" (substEnv env (mkCase i sym v' bs def))
      NMatch (Match _ _ _ vs bs) ->
        let !vs' = map' (eval' env) (toList vs)
         in match n env vs' bs
      NPi {} -> substEnv env n
      NUniv {} -> n
      NTyp (TypeConstr i sym args) -> mkTypeConstr i sym (map' (eval' env) args)
      NPrim {} -> n
      NDyn {} -> n
      NBot Bottom {..}
        | opts ^. evalOptionsNormalize || opts ^. evalOptionsNoFailure ->
            NBot Bottom {_bottomInfo, _bottomType = eval' env _bottomType}
        | otherwise -> evalError "bottom" n
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
          patmatch acc v PatWildcard {} =
            Just (v : acc)
          patmatch acc v@(NCtr (Constr _ tag args)) (PatConstr PatternConstr {..})
            | tag == _patternConstrTag =
                matchPatterns (v : acc) args _patternConstrArgs
          patmatch _ _ _ = Nothing
      [] ->
        evalError "no matching pattern" (substEnv env n)

    evalBody :: Info -> Binder -> Env -> Node -> Node -> Node
    evalBody i bi env v body
      | opts ^. evalOptionsNormalize
          && isTypePrim (bi ^. binderType)
          && not (isImmediate' v)
          && not (isFailNode v) =
          Closure env (mkLet i bi v body)
      | otherwise =
          eval' (v : env) body
    {-# INLINE evalBody #-}

    applyBuiltin :: Node -> Env -> BuiltinOp -> [Node] -> Node
    applyBuiltin n env opcode = case opcode of
      OpIntAdd -> binNumOp (+)
      OpIntSub -> binNumOp (-)
      OpIntMul -> binNumOp (*)
      OpIntLt -> binNumCmpOp (<)
      OpIntLe -> binNumCmpOp (<=)
      OpFieldAdd -> binFieldOp fieldAdd
      OpFieldSub -> binFieldOp fieldSub
      OpFieldMul -> binFieldOp fieldMul
      OpFieldDiv -> binFieldOp fieldDiv
      OpFieldFromInt -> fieldFromIntOp
      OpFieldToInt -> fieldToIntOp
      OpEq -> eqOp
      OpIntDiv -> divOp quot
      OpIntMod -> divOp rem
      OpShow -> showOp
      OpStrConcat -> strConcatOp
      OpStrToInt -> strToIntOp
      OpSeq -> seqOp
      OpFail -> failOp
      OpTrace -> traceOp
      OpAnomaGet -> anomaGetOp
      OpAnomaEncode -> anomaEncodeOp
      OpAnomaDecode -> anomaDecodeOp
      OpPoseidonHash -> poseidonHashOp
      OpEc -> ecOp
      OpRandomEcPoint -> randomEcPointOp
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
        divOp op = binOp' nodeFromInteger integerFromNode nonzeroIntegerFromNode $ \v1 v2 ->
          if
              | v2 == 0 ->
                  evalError "division by zero" (substEnv env n)
              | otherwise -> v1 `op` v2
        {-# INLINE divOp #-}

        binOp' :: (b -> Node) -> (Node -> Maybe a) -> (Node -> Maybe a) -> (a -> a -> b) -> [Node] -> Node
        binOp' toNode toA toA' op = binary $ \l r ->
          let !vl = eval' env l
              !vr = eval' env r
           in case (toA vl, toA' vr) of
                (Just v1, Just v2) ->
                  toNode (v1 `op` v2)
                _
                  | opts ^. evalOptionsNormalize || opts ^. evalOptionsNoFailure ->
                      mkBuiltinApp' opcode [vl, vr]
                  | otherwise ->
                      evalError "wrong operand type" n
        {-# INLINE binOp' #-}

        binOp :: (b -> Node) -> (Node -> Maybe a) -> (a -> a -> b) -> [Node] -> Node
        binOp toNode toA op = binOp' toNode toA toA op
        {-# INLINE binOp #-}

        binNumCmpOp :: (Integer -> Integer -> Bool) -> [Node] -> Node
        binNumCmpOp = binOp nodeFromBool integerFromNode
        {-# INLINE binNumCmpOp #-}

        binNumOp :: (Integer -> Integer -> Integer) -> [Node] -> Node
        binNumOp = binOp nodeFromInteger integerFromNode
        {-# INLINE binNumOp #-}

        binFieldOp :: (FField -> FField -> FField) -> [Node] -> Node
        binFieldOp = binOp nodeFromField fieldFromNode
        {-# INLINE binFieldOp #-}

        fieldFromIntOp :: [Node] -> Node
        fieldFromIntOp =
          unary $ \node ->
            let !v = eval' env node
             in nodeFromField $
                  fieldFromInteger (opts ^. evalOptionsFieldSize) $
                    fromMaybe (evalError "expected integer" v) $
                      integerFromNode v
        {-# INLINE fieldFromIntOp #-}

        fieldToIntOp :: [Node] -> Node
        fieldToIntOp =
          unary $ \node ->
            let !v = eval' env node
             in nodeFromInteger $
                  fieldToInteger $
                    fromMaybe (evalError "expected field element" v) $
                      fieldFromNode v
        {-# INLINE fieldToIntOp #-}

        eqOp :: [Node] -> Node
        eqOp
          | opts ^. evalOptionsNormalize =
              binOp nodeFromBool (\x -> if isDataValue x then Just x else Nothing) structEq
          | otherwise =
              binOp nodeFromBool Just structEq
        {-# INLINE eqOp #-}

        showOp :: [Node] -> Node
        showOp = unary $ \arg -> mkConstant' (ConstString (ppPrint (eval' env arg)))
        {-# INLINE showOp #-}

        strConcatOp :: [Node] -> Node
        strConcatOp = binary $ \arg1 arg2 ->
          case (eval' env arg1, eval' env arg2) of
            (NCst (Constant _ (ConstString s1)), NCst (Constant _ (ConstString s2))) ->
              mkConstant' (ConstString (s1 <> s2))
            _ ->
              evalError "string concatenation: argument not a string" n
        {-# INLINE strConcatOp #-}

        strToIntOp :: [Node] -> Node
        strToIntOp = unary $ \arg ->
          case eval' env arg of
            NCst (Constant _ (ConstString s)) ->
              case T.readMaybe (fromText s) of
                Just i ->
                  mkConstant' (ConstInteger i)
                Nothing
                  | opts ^. evalOptionsNormalize || opts ^. evalOptionsNoFailure ->
                      mkBuiltinApp' OpStrToInt [mkConstant' (ConstString s)]
                  | otherwise ->
                      evalError "string to integer: not an integer" n
            _ ->
              evalError "string conversion: argument not a string" n
        {-# INLINE strToIntOp #-}

        seqOp :: [Node] -> Node
        seqOp = binary $ \x y -> eval' env x `seq` eval' env y
        {-# INLINE seqOp #-}

        failOp :: [Node] -> Node
        failOp = unary $ \msg ->
          if
              | opts ^. evalOptionsNormalize || opts ^. evalOptionsNoFailure ->
                  mkBuiltinApp' OpFail [eval' env msg]
              | otherwise ->
                  Exception.throw (EvalError ("failure: " <> printNode (eval' env msg)) Nothing)
        {-# INLINE failOp #-}

        traceOp :: [Node] -> Node
        traceOp = unary $ \msg ->
          let !v = eval' env msg
           in if
                  | opts ^. evalOptionsSilent ->
                      v
                  | otherwise ->
                      unsafePerformIO (hPutStrLn herr (printNode v) >> return v)
        {-# INLINE traceOp #-}

        anomaGetOp :: [Node] -> Node
        anomaGetOp = unary $ \arg ->
          if
              | opts ^. evalOptionsNormalize || opts ^. evalOptionsNoFailure ->
                  mkBuiltinApp' OpAnomaGet [eval' env arg]
              | otherwise ->
                  err "unsupported builtin operation: OpAnomaGet"
        {-# INLINE anomaGetOp #-}

        anomaEncodeOp :: [Node] -> Node
        anomaEncodeOp = unary $ \arg ->
          if
              | opts ^. evalOptionsNormalize || opts ^. evalOptionsNoFailure ->
                  mkBuiltinApp' OpAnomaEncode [eval' env arg]
              | otherwise ->
                  err "unsupported builtin operation: OpAnomaEncode"
        {-# INLINE anomaEncodeOp #-}

        anomaDecodeOp :: [Node] -> Node
        anomaDecodeOp = unary $ \arg ->
          if
              | opts ^. evalOptionsNormalize || opts ^. evalOptionsNoFailure ->
                  mkBuiltinApp' OpAnomaDecode [eval' env arg]
              | otherwise ->
                  err "unsupported builtin operation: OpAnomaDecode"
        {-# INLINE anomaDecodeOp #-}

        poseidonHashOp :: [Node] -> Node
        poseidonHashOp = unary $ \arg ->
          if
              | opts ^. evalOptionsNormalize || opts ^. evalOptionsNoFailure ->
                  mkBuiltinApp' OpPoseidonHash [eval' env arg]
              | otherwise ->
                  err "unsupported builtin operation: OpPoseidonHash"
        {-# INLINE poseidonHashOp #-}

        ecOp :: [Node] -> Node
        ecOp = \case
          [arg1, arg2, arg3]
            | opts ^. evalOptionsNormalize || opts ^. evalOptionsNoFailure ->
                mkBuiltinApp' OpEc [eval' env arg1, eval' env arg2, eval' env arg3]
          _ ->
            err "unsupported builtin operation: OpEc"
        {-# INLINE ecOp #-}

        randomEcPointOp :: [Node] -> Node
        randomEcPointOp = \case
          []
            | opts ^. evalOptionsNormalize || opts ^. evalOptionsNoFailure ->
                mkBuiltinApp' OpRandomEcPoint []
          _ ->
            err "unsupported builtin operation: OpPoseidonHash"
        {-# INLINE randomEcPointOp #-}
    {-# INLINE applyBuiltin #-}

    nodeFromInteger :: Integer -> Node
    nodeFromInteger !int = mkConstant' (ConstInteger int)
    {-# INLINE nodeFromInteger #-}

    nodeFromField :: FField -> Node
    nodeFromField !fld = mkConstant' (ConstField fld)
    {-# INLINE nodeFromField #-}

    nodeFromBool :: Bool -> Node
    nodeFromBool b = mkConstr' (BuiltinTag tag) []
      where
        !tag
          | b = TagTrue
          | otherwise = TagFalse
    {-# INLINE nodeFromBool #-}

    integerFromNode :: Node -> Maybe Integer
    integerFromNode = \case
      NCst (Constant _ (ConstInteger int)) -> Just int
      _ -> Nothing
    {-# INLINE integerFromNode #-}

    nonzeroIntegerFromNode :: Node -> Maybe Integer
    nonzeroIntegerFromNode = \case
      NCst (Constant _ (ConstInteger int))
        | int /= 0 -> Just int
      _ -> Nothing
    {-# INLINE nonzeroIntegerFromNode #-}

    fieldFromNode :: Node -> Maybe FField
    fieldFromNode = \case
      NCst (Constant _ (ConstField fld)) -> Just fld
      _ -> Nothing
    {-# INLINE fieldFromNode #-}

    printNode :: Node -> Text
    printNode = \case
      NCst (Constant _ (ConstString s)) -> s
      v -> ppPrint v

    lookupContext :: Node -> Symbol -> Node
    lookupContext n sym =
      case HashMap.lookup sym ctx of
        Just n' -> eval' [] n'
        Nothing | opts ^. evalOptionsNormalize -> n
        Nothing -> evalError "symbol not defined" n
    {-# INLINE lookupContext #-}

    -- the Node arguments are assumed to be values
    goNormApp :: Info -> Node -> Node -> Node
    goNormApp i l r = case l of
      Closure env (NLet lt) ->
        Closure env (NLet (over letBody (\b -> mkApp i b r) lt))
      Closure env (NCase Case {..}) ->
        Closure
          env
          ( mkCase
              _caseInfo
              _caseInductive
              _caseValue
              (map (over caseBranchBody (\b -> mkApp i b r)) _caseBranches)
              (fmap (\b -> mkApp i b r) _caseDefault)
          )
      NBlt (BuiltinApp {..})
        | _builtinAppOp == OpFail ->
            l
      _ ->
        mkApp i l r

    goNormCase :: Env -> Info -> Symbol -> Node -> [CaseBranch] -> Maybe Node -> Node
    goNormCase env i sym v bs def = case v of
      Closure env' (NLet Let {..}) ->
        Closure
          env'
          ( mkLet
              _letInfo
              (_letItem ^. letItemBinder)
              (_letItem ^. letItemValue)
              (goBody _letBody)
          )
      Closure env' (NCase Case {..})
        | not (isCaseBoolean bs) ->
            Closure
              env'
              ( mkCase
                  _caseInfo
                  _caseInductive
                  _caseValue
                  (map (over caseBranchBody goBody) _caseBranches)
                  (fmap goBody _caseDefault)
              )
      _ ->
        Closure env (mkCase i sym v bs def)
      where
        goBody :: Node -> Node
        goBody b =
          mkCase i sym b (map (substEnvInBranch env) bs) (fmap (substEnv env) def)

-- Evaluate `node` and interpret the builtin IO actions.
hEvalIO' :: EvalOptions -> Handle -> Handle -> Handle -> IdentContext -> Env -> Node -> IO Node
hEvalIO' opts herr hin hout ctx env node =
  let node' = geval opts herr ctx env node
   in case node' of
        NCtr (Constr _ (BuiltinTag TagReturn) [x]) ->
          return x
        NCtr (Constr _ (BuiltinTag TagBind) [x, f]) -> do
          x' <- hEvalIO' opts herr hin hout ctx env x
          hEvalIO' opts herr hin hout ctx env (mkApp Info.empty f x')
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

hEvalIO :: Handle -> Handle -> Handle -> IdentContext -> Env -> Node -> IO Node
hEvalIO = hEvalIO' defaultEvalOptions

evalIO :: IdentContext -> Env -> Node -> IO Node
evalIO = hEvalIO stderr stdin stdout

doEval ::
  forall r.
  (MonadIO (Sem r)) =>
  Maybe Natural ->
  Bool ->
  Interval ->
  InfoTable ->
  Node ->
  Sem r (Either CoreError Node)
doEval mfsize noIO loc tab node
  | noIO = catchEvalError loc (eval stderr (tab ^. identContext) [] node)
  | otherwise = liftIO (catchEvalErrorIO loc (hEvalIO' opts stderr stdin stdout (tab ^. identContext) [] node))
  where
    opts =
      maybe
        defaultEvalOptions
        (\fsize -> defaultEvalOptions {_evalOptionsFieldSize = fsize})
        mfsize

doEvalIO ::
  Maybe Natural ->
  Bool ->
  Interval ->
  InfoTable ->
  Node ->
  IO (Either CoreError Node)
doEvalIO mfsize noIO i tab node = runM (doEval mfsize noIO i tab node)

-- | Catch EvalError and convert it to CoreError. Needs a default location in case
-- no location is available in EvalError.
catchEvalError :: (MonadIO m) => Location -> a -> m (Either CoreError a)
catchEvalError loc a =
  liftIO $
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
    { _coreErrorMsg = ppOutput $ "evaluation error: " <> pretty _evalErrorMsg,
      _coreErrorNode = _evalErrorNode,
      _coreErrorLoc = fromMaybe loc (lookupLocation =<< _evalErrorNode)
    }
