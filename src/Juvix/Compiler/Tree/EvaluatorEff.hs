module Juvix.Compiler.Tree.EvaluatorEff where

import Control.Exception qualified as Exception
import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Tree.Data.InfoTable
import Juvix.Compiler.Tree.Error
import Juvix.Compiler.Tree.Evaluator (EvalError (..))
import Juvix.Compiler.Tree.Extra.Base
import Juvix.Compiler.Tree.Language
import Juvix.Compiler.Tree.Language.Value
import Juvix.Compiler.Tree.Pretty
import Text.Read qualified as T
import Effectful.Writer.Static.Loca

data EvalCtx = EvalCtx
  { _evalCtxArgs :: [Value],
    _evalCtxTemp :: BL.BinderList Value
  }

makeLenses ''EvalCtx

emptyEvalCtx :: EvalCtx
emptyEvalCtx =
  EvalCtx
    { _evalCtxArgs = [],
      _evalCtxTemp = mempty
    }

eval :: (Members '[Output Value, Error EvalError] r) => InfoTable -> Node -> Sem r Value
eval tab = runReader emptyEvalCtx . eval'
  where
    eval' :: forall r'. (Members '[Output Value, Reader EvalCtx, Error EvalError] r') => Node -> Sem r' Value
    eval' node = case node of
      Binop x -> goBinop x
      Unop x -> goUnop x
      Const c -> return (goConstant c)
      MemRef x -> goMemRef x
      AllocConstr x -> goAllocConstr x
      AllocClosure x -> goAllocClosure x
      ExtendClosure x -> goExtendClosure x
      Call x -> goCall x
      CallClosures x -> goCallClosures x
      Branch x -> goBranch x
      Case x -> goCase x
      Save x -> goSave x
      where
        evalError :: Text -> Sem r' a
        evalError msg =
          Exception.throw (EvalError (getNodeLocation node) msg)

        goBinop :: NodeBinop -> Sem r' Value
        goBinop NodeBinop {..} = do
          arg1 <- eval' _nodeBinopArg1
          arg2 <- eval' _nodeBinopArg2
          case _nodeBinopOpcode of
            IntAdd -> goIntBinop (+) arg1 arg2
            IntSub -> goIntBinop (-) arg1 arg2
            IntMul -> goIntBinop (*) arg1 arg2
            IntDiv
              | arg2 == ValInteger 0 -> evalError "division by zero"
              | otherwise -> goIntBinop quot arg1 arg2
            IntMod
              | arg2 == ValInteger 0 -> evalError "division by zero"
              | otherwise -> goIntBinop rem arg1 arg2
            IntLe -> goIntCmpBinop (<=) arg1 arg2
            IntLt -> goIntCmpBinop (<) arg1 arg2
            ValEq -> return (ValBool (arg1 == arg2))
            StrConcat -> goStrConcat arg1 arg2
            OpSeq -> return arg2

        goIntBinop :: (Integer -> Integer -> Integer) -> Value -> Value -> Sem r' Value
        goIntBinop f v1 v2 = case (v1, v2) of
          (ValInteger i1, ValInteger i2) -> return (ValInteger (f i1 i2))
          _ -> evalError "expected two integer arguments"

        goIntCmpBinop :: (Integer -> Integer -> Bool) -> Value -> Value -> Sem r' Value
        goIntCmpBinop f v1 v2 = case (v1, v2) of
          (ValInteger i1, ValInteger i2) -> return (ValBool (f i1 i2))
          _ -> evalError "expected two integer arguments"

        goStrConcat :: Value -> Value -> Sem r' Value
        goStrConcat v1 v2 = case (v1, v2) of
          (ValString s1, ValString s2) -> return (ValString (s1 <> s2))
          _ -> evalError "expected two string arguments"

        goUnop :: NodeUnop -> Sem r' Value
        goUnop NodeUnop {..} = do
          v <- eval' _nodeUnopArg
          case _nodeUnopOpcode of
            OpShow -> return (ValString (printValue tab v))
            OpStrToInt -> goStringUnop strToInt v
            OpTrace -> goTrace v
            OpFail -> goFail v
            OpArgsNum -> goArgsNum v

        strToInt :: Text -> Sem r' Value
        strToInt s = case T.readMaybe (fromText s) of
          Just i -> return (ValInteger i)
          Nothing -> evalError "string to integer: not an integer"

        goStringUnop :: (Text -> Sem r' Value) -> Value -> Sem r' Value
        goStringUnop f = \case
          ValString s -> f s
          _ -> evalError "expected a string argument"

        goFail :: Value -> Sem r' Value
        goFail v = evalError ("failure: " <> printValue tab v)

        goArgsNum :: Value -> Sem r' Value
        goArgsNum = \case
          ValClosure Closure {..} -> return (ValInteger (fromIntegral argsNum))
            where
              fi = lookupFunInfo tab _closureSymbol
              argsNum = fi ^. functionArgsNum - length _closureArgs
          _ ->
            evalError "expected a closure"

        goTrace :: Value -> Sem r' Value
        goTrace v = output v $> v

        goConstant :: NodeConstant -> Value
        goConstant NodeConstant {..} = case _nodeConstant of
          ConstInt i -> ValInteger i
          ConstBool b -> ValBool b
          ConstString s -> ValString s
          ConstUnit -> ValUnit
          ConstVoid -> ValVoid

        askTemp :: Sem r' (BL.BinderList Value)
        askTemp = asks (^. evalCtxTemp)

        askArgs :: Sem r' [Value]
        askArgs = asks (^. evalCtxArgs)

        goMemRef :: NodeMemRef -> Sem r' Value
        goMemRef NodeMemRef {..} = case _nodeMemRef of
          DRef r -> goDirectRef r
          ConstrRef r -> goField r

        goDirectRef :: DirectRef -> Sem r' Value
        goDirectRef = \case
          ArgRef OffsetRef {..} ->
            (!! _offsetRefOffset) <$> askArgs
          TempRef RefTemp {_refTempOffsetRef = OffsetRef {..}} ->
            BL.lookupLevel _offsetRefOffset <$> askTemp

        goField :: Field -> Sem r' Value
        goField Field {..} = do
          d <- goDirectRef _fieldRef
          case d of
            ValConstr Constr {..} -> return (_constrArgs !! _fieldOffset)
            _ -> evalError "expected a constructor"

        goAllocConstr :: NodeAllocConstr -> Sem r' Value
        goAllocConstr NodeAllocConstr {..} = do
          vs <- mapM eval' _nodeAllocConstrArgs
          return
            ( ValConstr
                Constr
                  { _constrTag = _nodeAllocConstrTag,
                    _constrArgs = vs
                  }
            )

        goAllocClosure :: NodeAllocClosure -> Sem r' Value
        goAllocClosure NodeAllocClosure {..} = do
          vs <- mapM eval' _nodeAllocClosureArgs
          return
            ( ValClosure
                Closure
                  { _closureSymbol = _nodeAllocClosureFunSymbol,
                    _closureArgs = vs
                  }
            )

        goExtendClosure :: NodeExtendClosure -> Sem r' Value
        goExtendClosure NodeExtendClosure {..} = do
          fun <- eval' _nodeExtendClosureFun
          case fun of
            ValClosure Closure {..} -> do
              vs <- mapM eval' (toList _nodeExtendClosureArgs)
              return
                ( ValClosure
                    Closure
                      { _closureSymbol,
                        _closureArgs = _closureArgs ++ vs
                      }
                )
            _ -> evalError "expected a closure"

        goCall :: NodeCall -> Sem r' Value
        goCall NodeCall {..} = case _nodeCallType of
          CallFun sym -> doCall sym [] _nodeCallArgs
          CallClosure cl -> doCallClosure cl _nodeCallArgs

        withCtx :: EvalCtx -> Sem r' a -> Sem r' a
        withCtx = local . const

        doCall :: Symbol -> [Value] -> [Node] -> Sem r' Value
        doCall sym clArgs as = do
          vs <- mapM eval' as
          let fi = lookupFunInfo tab sym
              vs' = clArgs ++ vs
           in if
                  | length vs' == fi ^. functionArgsNum -> do
                      let ctx' =
                            EvalCtx
                              { _evalCtxArgs = vs',
                                _evalCtxTemp = mempty
                              }
                      withCtx ctx' (eval' (fi ^. functionCode))
                  | otherwise ->
                      evalError "wrong number of arguments"

        doCallClosure :: Node -> [Node] -> Sem r' Value
        doCallClosure cl cargs = do
          cl' <- eval' cl
          case cl' of
            ValClosure Closure {..} ->
              doCall _closureSymbol _closureArgs cargs
            _ ->
              evalError "expected a closure"

        goCallClosures :: NodeCallClosures -> Sem r' Value
        goCallClosures NodeCallClosures {..} = do
          vs <- mapM eval' (toList _nodeCallClosuresArgs)
          cl' <- eval' _nodeCallClosuresFun
          go cl' vs
          where
            go :: Value -> [Value] -> Sem r' Value
            go cl vs = case cl of
              ValClosure Closure {..}
                | argsNum == n -> do
                    let ctx' =
                          EvalCtx
                            { _evalCtxArgs = vs',
                              _evalCtxTemp = mempty
                            }
                    withCtx ctx' (eval' body)
                | argsNum < n -> do
                    let ctx' =
                          EvalCtx
                            { _evalCtxArgs = take argsNum vs',
                              _evalCtxTemp = mempty
                            }

                    body' <- withCtx ctx' (eval' body)
                    go body' (drop argsNum vs')
                | otherwise ->
                    return
                      ( ValClosure
                          Closure
                            { _closureSymbol,
                              _closureArgs = vs'
                            }
                      )
                where
                  fi = lookupFunInfo tab _closureSymbol
                  argsNum = fi ^. functionArgsNum
                  vs' = _closureArgs ++ vs
                  n = length vs'
                  body = fi ^. functionCode
              _ ->
                evalError "expected a closure"

        goBranch :: NodeBranch -> Sem r' Value
        goBranch NodeBranch {..} = do
          arg' <- eval' _nodeBranchArg
          br <- case arg' of
            ValBool True -> return _nodeBranchTrue
            ValBool False -> return _nodeBranchFalse
            _ -> evalError "expected a boolean"
          eval' br

        goCase :: NodeCase -> Sem r' Value
        goCase NodeCase {..} = do
          arg' <- eval' _nodeCaseArg
          case arg' of
            v@(ValConstr Constr {..}) ->
              case find (\CaseBranch {..} -> _caseBranchTag == _constrTag) _nodeCaseBranches of
                Just CaseBranch {..} -> goCaseBranch v _caseBranchSave _caseBranchBody
                Nothing -> do
                  def <- maybe (evalError "no matching branch") return _nodeCaseDefault
                  goCaseBranch v False def
            _ ->
              evalError "expected a constructor"

        withExtendedTemp :: Value -> Sem r' a -> Sem r' a
        withExtendedTemp v m = do
          ctx <- ask
          withCtx (over evalCtxTemp (BL.cons v) ctx) m

        goCaseBranch :: Value -> Bool -> Node -> Sem r' Value
        goCaseBranch v bSave body
          | bSave = withExtendedTemp v (eval' body)
          | otherwise = eval' body

        goSave :: NodeSave -> Sem r' Value
        goSave NodeSave {..} = do
          v <- eval' _nodeSaveArg
          withExtendedTemp v (eval' _nodeSaveBody)

printValue :: InfoTable -> Value -> Text
printValue tab = \case
  ValString s -> s
  v -> ppPrint tab v

valueToNode :: Value -> Node
valueToNode = \case
  ValInteger i -> mkConst $ ConstInt i
  ValBool b -> mkConst $ ConstBool b
  ValString s -> mkConst $ ConstString s
  ValUnit -> mkConst ConstUnit
  ValVoid -> mkConst ConstVoid
  ValConstr Constr {..} ->
    AllocConstr
      NodeAllocConstr
        { _nodeAllocConstrInfo = mempty,
          _nodeAllocConstrTag = _constrTag,
          _nodeAllocConstrArgs = map valueToNode _constrArgs
        }
  ValClosure Closure {..} ->
    AllocClosure
      NodeAllocClosure
        { _nodeAllocClosureInfo = mempty,
          _nodeAllocClosureFunSymbol = _closureSymbol,
          _nodeAllocClosureArgs = map valueToNode _closureArgs
        }

hEvalIOEither ::
  forall m.
  (MonadIO m) =>
  Handle ->
  Handle ->
  InfoTable ->
  FunctionInfo ->
  m (Either TreeError Value)
hEvalIOEither hin hout infoTable funInfo = do
  let x = do
        v <- eval infoTable (funInfo ^. functionCode)
        hRunIO hin hout infoTable v
  let handleTrace = liftIO . hPutStrLn hout . printValue infoTable
  liftIO
    . runM
    . runError @TreeError
    . mapError toTreeError
    . runOutputSem handleTrace
    $ x

-- | Interpret IO actions.
hRunIO :: forall r. (Members '[Embed IO, Error EvalError, Output Value] r) => Handle -> Handle -> InfoTable -> Value -> Sem r Value
hRunIO hin hout infoTable = \case
  ValConstr (Constr (BuiltinTag TagReturn) [x]) -> return x
  ValConstr (Constr (BuiltinTag TagBind) [x, f]) -> do
    x' <- hRunIO hin hout infoTable x
    let code =
          CallClosures
            NodeCallClosures
              { _nodeCallClosuresInfo = mempty,
                _nodeCallClosuresFun = valueToNode f,
                _nodeCallClosuresArgs = valueToNode x' :| []
              }
    res <- eval infoTable code
    hRunIO hin hout infoTable res
  ValConstr (Constr (BuiltinTag TagWrite) [ValString s]) -> do
    liftIO $ hPutStr hout s
    return ValVoid
  ValConstr (Constr (BuiltinTag TagWrite) [arg]) -> do
    liftIO $ hPutStr hout (ppPrint infoTable arg)
    return ValVoid
  ValConstr (Constr (BuiltinTag TagReadLn) []) -> do
    liftIO $ hFlush hout
    s <- liftIO $ hGetLine hin
    return (ValString s)
  val ->
    return val

-- | Catch EvalError and convert it to TreeError.
catchEvalErrorIO :: IO a -> IO (Either TreeError a)
catchEvalErrorIO ma =
  Exception.catch
    (Exception.evaluate ma >>= \ma' -> Right <$> ma')
    (\(ex :: EvalError) -> return (Left (toTreeError ex)))

toTreeError :: EvalError -> TreeError
toTreeError EvalError {..} =
  TreeError
    { _treeErrorMsg = "evaluation error: " <> _evalErrorMsg,
      _treeErrorLoc = _evalErrorLocation
    }
