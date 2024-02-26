module Juvix.Compiler.Tree.EvaluatorEff (eval, hEvalIOEither) where

import Control.Exception qualified as Exception
import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Tree.Data.InfoTable
import Juvix.Compiler.Tree.Error
import Juvix.Compiler.Tree.Evaluator (EvalError (..), toTreeError, valueToNode)
import Juvix.Compiler.Tree.Evaluator.Builtins
import Juvix.Compiler.Tree.Extra.Base
import Juvix.Compiler.Tree.Language hiding (Error, Members, Output, Reader, Sem, ask, asks, local, mapError, output, runError, runReader)
import Juvix.Compiler.Tree.Language.Value
import Juvix.Compiler.Tree.Pretty
import Juvix.Prelude.BaseEffectful

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
      Constant c -> return (goConstant c)
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

        eitherToError :: Either Text Value -> Sem r' Value
        eitherToError = \case
          Left err -> evalError err
          Right v -> return v

        goBinop :: NodeBinop -> Sem r' Value
        goBinop NodeBinop {..} = do
          arg1 <- eval' _nodeBinopArg1
          arg2 <- eval' _nodeBinopArg2
          case _nodeBinopOpcode of
            PrimBinop op -> eitherToError $ evalBinop op arg1 arg2
            OpSeq -> return arg2

        goUnop :: NodeUnop -> Sem r' Value
        goUnop NodeUnop {..} = do
          v <- eval' _nodeUnopArg
          case _nodeUnopOpcode of
            PrimUnop op -> eitherToError $ evalUnop tab op v
            OpTrace -> goTrace v
            OpFail -> goFail v

        goFail :: Value -> Sem r' Value
        goFail v = evalError ("failure: " <> printValue tab v)

        goTrace :: Value -> Sem r' Value
        goTrace v = output v $> v

        goConstant :: NodeConstant -> Value
        goConstant NodeConstant {..} = constantToValue _nodeConstant

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

hEvalIOEither ::
  forall m.
  (MonadIO m) =>
  Handle ->
  Handle ->
  InfoTable ->
  FunctionInfo ->
  m (Either TreeError Value)
hEvalIOEither hin hout infoTable funInfo = do
  let x :: Sem '[Output Value, Error EvalError, Error TreeError, IOE] Value
      x = do
        v <- eval infoTable (funInfo ^. functionCode)
        hRunIO hin hout infoTable v
  let handleTrace :: forall q. (MonadIO q) => Value -> q ()
      handleTrace = hPutStrLn hout . printValue infoTable
  liftIO
    . runEff
    . runError @TreeError
    . mapError toTreeError
    . runOutputSem handleTrace
    $ x

-- | Interpret IO actions.
hRunIO :: forall r. (Members '[IOE, Error EvalError, Output Value] r) => Handle -> Handle -> InfoTable -> Value -> Sem r Value
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
    hPutStr hout s
    return ValVoid
  ValConstr (Constr (BuiltinTag TagWrite) [arg]) -> do
    hPutStr hout (ppPrint infoTable arg)
    return ValVoid
  ValConstr (Constr (BuiltinTag TagReadLn) []) -> do
    liftIO $ hFlush hout
    s <- liftIO $ hGetLine hin
    return (ValString s)
  val ->
    return val
