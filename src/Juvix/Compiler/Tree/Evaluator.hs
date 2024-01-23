{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted extensions" #-}
{-# HLINT ignore "Avoid restricted flags" #-}
module Juvix.Compiler.Tree.Evaluator where

import Control.Exception qualified as Exception
import GHC.IO (unsafePerformIO)
import GHC.Show qualified as S
import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Tree.Data.InfoTable
import Juvix.Compiler.Tree.Error
import Juvix.Compiler.Tree.Language
import Juvix.Compiler.Tree.Language.Value
import Juvix.Compiler.Tree.Pretty
import Text.Read qualified as T

newtype EvalError = EvalError
  { _evalErrorMsg :: Text
  }

makeLenses ''EvalError

instance Show EvalError where
  show :: EvalError -> String
  show EvalError {..} =
    "evaluation error: "
      ++ fromText _evalErrorMsg

instance Exception.Exception EvalError

eval :: InfoTable -> Node -> Value
eval tab = eval' [] mempty
  where
    eval' :: [Value] -> BL.BinderList Value -> Node -> Value
    eval' args temps node = case node of
      Binop x -> goBinop x
      Unop x -> goUnop x
      Const c -> goConstant c
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
        evalError :: Text -> a
        evalError msg =
          Exception.throw (EvalError (msg <> ": " <> ppTrace tab node))

        goBinop :: NodeBinop -> Value
        goBinop NodeBinop {..} =
          let !arg1 = eval' args temps _nodeBinopArg1
              !arg2 = eval' args temps _nodeBinopArg2
           in case _nodeBinopOpcode of
                IntAdd -> goIntBinop (+) arg1 arg2
                IntSub -> goIntBinop (-) arg1 arg2
                IntMul -> goIntBinop (*) arg1 arg2
                IntDiv -> goIntBinop quot arg1 arg2
                IntMod -> goIntBinop rem arg1 arg2
                IntLe -> goIntCmpBinop (<=) arg1 arg2
                IntLt -> goIntCmpBinop (<) arg1 arg2
                ValEq
                  | arg1 == arg2 -> ValBool True
                  | otherwise -> ValBool False
                StrConcat -> goStrConcat arg1 arg2
                OpSeq -> arg2

        goIntBinop :: (Integer -> Integer -> Integer) -> Value -> Value -> Value
        goIntBinop f v1 v2 = case (v1, v2) of
          (ValInteger i1, ValInteger i2) -> ValInteger (f i1 i2)
          _ -> evalError "expected two integer arguments"

        goIntCmpBinop :: (Integer -> Integer -> Bool) -> Value -> Value -> Value
        goIntCmpBinop f v1 v2 = case (v1, v2) of
          (ValInteger i1, ValInteger i2) -> ValBool (f i1 i2)
          _ -> evalError "expected two integer arguments"

        goStrConcat :: Value -> Value -> Value
        goStrConcat v1 v2 = case (v1, v2) of
          (ValString s1, ValString s2) -> ValString (s1 <> s2)
          _ -> evalError "expected two string arguments"

        goUnop :: NodeUnop -> Value
        goUnop NodeUnop {..} =
          let !v = eval' args temps _nodeUnopArg
           in case _nodeUnopOpcode of
                OpShow -> ValString (ppTrace tab v)
                OpStrToInt -> goStringUnop strToInt v
                OpTrace -> goTrace v
                OpFail -> goFail v
                OpArgsNum -> goArgsNum v

        strToInt :: Text -> Value
        strToInt s = case T.readMaybe (fromText s) of
          Just i ->
            ValInteger i
          Nothing ->
            evalError "string to integer: not an integer"

        goStringUnop :: (Text -> Value) -> Value -> Value
        goStringUnop f = \case
          ValString s -> f s
          _ -> evalError "expected a string argument"

        goFail :: Value -> Value
        goFail v = evalError ("failure: " <> ppPrint tab v)

        goArgsNum :: Value -> Value
        goArgsNum = \case
          ValClosure Closure {..} ->
            ValInteger (fromIntegral argsNum)
            where
              fi = lookupFunInfo tab _closureSymbol
              argsNum = fi ^. functionArgsNum - length _closureArgs
          _ ->
            evalError "expected a closure"

        goTrace :: Value -> Value
        goTrace v = unsafePerformIO (putStrLn (ppPrint tab v) >> return v)

        goConstant :: Constant -> Value
        goConstant = \case
          ConstInt i -> ValInteger i
          ConstBool b -> ValBool b
          ConstString s -> ValString s
          ConstUnit -> ValUnit
          ConstVoid -> ValVoid

        goMemRef :: MemRef -> Value
        goMemRef = \case
          DRef r -> goDirectRef r
          ConstrRef r -> goField r

        goDirectRef :: DirectRef -> Value
        goDirectRef = \case
          ArgRef OffsetRef {..} ->
            args !! _offsetRefOffset
          TempRef RefTemp {_refTempOffsetRef = OffsetRef {..}} ->
            BL.lookupLevel _offsetRefOffset temps

        goField :: Field -> Value
        goField Field {..} = case goDirectRef _fieldRef of
          ValConstr Constr {..} -> _constrArgs !! _fieldOffset
          _ -> evalError "expected a constructor"

        goAllocConstr :: NodeAllocConstr -> Value
        goAllocConstr NodeAllocConstr {..} =
          let !vs = map' (eval' args temps) _nodeAllocConstrArgs
           in ValConstr
                Constr
                  { _constrTag = _nodeAllocConstrTag,
                    _constrArgs = vs
                  }

        goAllocClosure :: NodeAllocClosure -> Value
        goAllocClosure NodeAllocClosure {..} =
          let !vs = map' (eval' args temps) _nodeAllocClosureArgs
           in ValClosure
                Closure
                  { _closureSymbol = _nodeAllocClosureFunSymbol,
                    _closureArgs = vs
                  }

        goExtendClosure :: NodeExtendClosure -> Value
        goExtendClosure NodeExtendClosure {..} =
          case eval' args temps _nodeExtendClosureFun of
            ValClosure Closure {..} ->
              let !vs = map' (eval' args temps) (toList _nodeExtendClosureArgs)
               in ValClosure
                    Closure
                      { _closureSymbol,
                        _closureArgs = _closureArgs ++ vs
                      }
            _ -> evalError "expected a closure"

        goCall :: NodeCall -> Value
        goCall NodeCall {..} = case _nodeCallType of
          CallFun sym -> doCall sym [] _nodeCallArgs
          CallClosure cl -> doCallClosure cl _nodeCallArgs

        doCall :: Symbol -> [Value] -> [Node] -> Value
        doCall sym vs0 as =
          let !vs = map' (eval' args temps) as
              fi = lookupFunInfo tab sym
              vs' = vs0 ++ vs
           in if
                  | length vs' == fi ^. functionArgsNum ->
                      eval' vs' mempty (fi ^. functionCode)
                  | otherwise ->
                      evalError "wrong number of arguments"

        doCallClosure :: Node -> [Node] -> Value
        doCallClosure cl cargs = case eval' args temps cl of
          ValClosure Closure {..} ->
            doCall _closureSymbol _closureArgs cargs
          _ ->
            evalError "expected a closure"

        goCallClosures :: NodeCallClosures -> Value
        goCallClosures NodeCallClosures {..} =
          let !vs = map' (eval' args temps) _nodeCallClosuresArgs
           in go (eval' args temps _nodeCallClosuresFun) vs
          where
            go :: Value -> [Value] -> Value
            go cl vs = case cl of
              ValClosure Closure {..}
                | argsNum == n ->
                    eval' vs' mempty body
                | argsNum < n ->
                    go (eval' (take argsNum vs') mempty body) (drop argsNum vs')
                | otherwise ->
                    ValClosure
                      Closure
                        { _closureSymbol,
                          _closureArgs = vs'
                        }
                where
                  fi = lookupFunInfo tab _closureSymbol
                  argsNum = fi ^. functionArgsNum
                  vs' = _closureArgs ++ vs
                  n = length vs'
                  body = fi ^. functionCode
              _ ->
                evalError "expected a closure"

        goBranch :: NodeBranch -> Value
        goBranch NodeBranch {..} =
          case eval' args temps _nodeBranchArg of
            ValBool True -> eval' args temps _nodeBranchTrue
            ValBool False -> eval' args temps _nodeBranchFalse
            _ -> evalError "expected a boolean"

        goCase :: NodeCase -> Value
        goCase NodeCase {..} =
          case eval' args temps _nodeCaseArg of
            v@(ValConstr Constr {..}) ->
              case find (\CaseBranch {..} -> _caseBranchTag == _constrTag) _nodeCaseBranches of
                Just CaseBranch {..} -> goCaseBranch v _caseBranchSave _caseBranchBody
                Nothing -> goCaseBranch v False (fromMaybe (evalError "no matching branch") _nodeCaseDefault)
            _ ->
              evalError "expected a constructor"

        goCaseBranch :: Value -> Bool -> Node -> Value
        goCaseBranch v bSave body
          | bSave = eval' args (BL.cons v temps) body
          | otherwise = eval' args temps body

        goSave :: NodeSave -> Value
        goSave NodeSave {..} =
          let !v = eval' args temps _nodeSaveArg
           in eval' args (BL.cons v temps) _nodeSaveBody

-- | Catch EvalError and convert it to TreeError.
catchEvalError :: (MonadIO m) => a -> m (Either TreeError a)
catchEvalError a =
  liftIO $
    Exception.catch
      (Exception.evaluate a <&> Right)
      (\(ex :: EvalError) -> return (Left (toTreeError ex)))

toTreeError :: EvalError -> TreeError
toTreeError EvalError {..} =
  TreeError
    { _treeErrorMsg = "evaluation error: " <> _evalErrorMsg,
      _treeErrorLoc = Nothing
    }