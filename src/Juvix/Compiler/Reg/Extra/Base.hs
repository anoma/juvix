module Juvix.Compiler.Reg.Extra.Base where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Reg.Language

getResultVar :: Instruction -> Maybe VarRef
getResultVar = \case
  Binop x -> Just $ x ^. instrBinopResult
  Unop x -> Just $ x ^. instrUnopResult
  Cairo x -> Just $ x ^. instrCairoResult
  Assign x -> Just $ x ^. instrAssignResult
  Alloc x -> Just $ x ^. instrAllocResult
  AllocClosure x -> Just $ x ^. instrAllocClosureResult
  ExtendClosure x -> Just $ x ^. instrExtendClosureResult
  Call x -> Just $ x ^. instrCallResult
  CallClosures x -> Just $ x ^. instrCallClosuresResult
  _ -> Nothing

setResultVar :: Instruction -> VarRef -> Instruction
setResultVar instr vref = case instr of
  Binop x -> Binop $ set instrBinopResult vref x
  Unop x -> Unop $ set instrUnopResult vref x
  Cairo x -> Cairo $ set instrCairoResult vref x
  Assign x -> Assign $ set instrAssignResult vref x
  Alloc x -> Alloc $ set instrAllocResult vref x
  AllocClosure x -> AllocClosure $ set instrAllocClosureResult vref x
  ExtendClosure x -> ExtendClosure $ set instrExtendClosureResult vref x
  Call x -> Call $ set instrCallResult vref x
  CallClosures x -> CallClosures $ set instrCallClosuresResult vref x
  _ -> impossible

getOutVar :: Instruction -> Maybe VarRef
getOutVar = \case
  If x -> x ^. instrIfOutVar
  Branch x -> x ^. instrBranchOutVar
  Case x -> x ^. instrCaseOutVar
  _ -> Nothing

overValueRefs'' :: forall m. (Monad m) => (VarRef -> m Value) -> Instruction -> m Instruction
overValueRefs'' f = \case
  Binop x -> Binop <$> goBinop x
  Unop x -> Unop <$> goUnop x
  Cairo x -> Cairo <$> goCairo x
  Assign x -> Assign <$> goAssign x
  Alloc x -> Alloc <$> goAlloc x
  AllocClosure x -> AllocClosure <$> goAllocClosure x
  ExtendClosure x -> ExtendClosure <$> goExtendClosure x
  Call x -> Call <$> goCall x
  CallClosures x -> CallClosures <$> goCallClosures x
  TailCall x -> TailCall <$> goTailCall x
  TailCallClosures x -> TailCallClosures <$> goTailCallClosures x
  Return x -> Return <$> goReturn x
  If x -> If <$> goIf x
  Branch x -> Branch <$> goBranch x
  Case x -> Case <$> goCase x
  Trace x -> Trace <$> goTrace x
  Dump -> return Dump
  Failure x -> Failure <$> goFailure x
  Prealloc x -> Prealloc <$> goPrealloc x
  Nop -> return Nop
  Block x -> Block <$> goBlock x
  where
    fromVarRef :: Value -> VarRef
    fromVarRef = \case
      VRef r -> r
      _ -> impossible

    goConstrField :: ConstrField -> m ConstrField
    goConstrField = overM constrFieldRef (fmap fromVarRef . f)

    goValue :: Value -> m Value
    goValue = \case
      ValConst c -> return $ ValConst c
      CRef x -> CRef <$> goConstrField x
      VRef x -> f x

    goBinop :: InstrBinop -> m InstrBinop
    goBinop InstrBinop {..} = do
      arg1 <- goValue _instrBinopArg1
      arg2 <- goValue _instrBinopArg2
      return
        InstrBinop
          { _instrBinopArg1 = arg1,
            _instrBinopArg2 = arg2,
            ..
          }

    goUnop :: InstrUnop -> m InstrUnop
    goUnop = overM instrUnopArg goValue

    goCairo :: InstrCairo -> m InstrCairo
    goCairo = overM instrCairoArgs (mapM goValue)

    goAssign :: InstrAssign -> m InstrAssign
    goAssign = overM instrAssignValue goValue

    goAlloc :: InstrAlloc -> m InstrAlloc
    goAlloc = overM instrAllocArgs (mapM goValue)

    goAllocClosure :: InstrAllocClosure -> m InstrAllocClosure
    goAllocClosure = overM instrAllocClosureArgs (mapM goValue)

    goExtendClosure :: InstrExtendClosure -> m InstrExtendClosure
    goExtendClosure InstrExtendClosure {..} = do
      val <- f _instrExtendClosureValue
      args <- mapM goValue _instrExtendClosureArgs
      return
        InstrExtendClosure
          { _instrExtendClosureValue = fromVarRef val,
            _instrExtendClosureArgs = args,
            ..
          }

    goCallType :: CallType -> m CallType
    goCallType = \case
      CallFun sym -> return $ CallFun sym
      CallClosure cl -> do
        val <- f cl
        return $ CallClosure (fromVarRef val)

    goCall :: InstrCall -> m InstrCall
    goCall InstrCall {..} = do
      ct <- goCallType _instrCallType
      args <- mapM goValue _instrCallArgs
      return $
        InstrCall
          { _instrCallType = ct,
            _instrCallArgs = args,
            ..
          }

    goCallClosures :: InstrCallClosures -> m InstrCallClosures
    goCallClosures InstrCallClosures {..} = do
      args <- mapM goValue _instrCallClosuresArgs
      val <- f _instrCallClosuresValue
      return $
        InstrCallClosures
          { _instrCallClosuresArgs = args,
            _instrCallClosuresValue = fromVarRef val,
            ..
          }

    goTailCall :: InstrTailCall -> m InstrTailCall
    goTailCall InstrTailCall {..} = do
      ct <- goCallType _instrTailCallType
      args <- mapM goValue _instrTailCallArgs
      return
        InstrTailCall
          { _instrTailCallType = ct,
            _instrTailCallArgs = args,
            ..
          }

    goTailCallClosures :: InstrTailCallClosures -> m InstrTailCallClosures
    goTailCallClosures InstrTailCallClosures {..} = do
      val <- f _instrTailCallClosuresValue
      args <- mapM goValue _instrTailCallClosuresArgs
      return
        InstrTailCallClosures
          { _instrTailCallClosuresValue = fromVarRef val,
            _instrTailCallClosuresArgs = args,
            ..
          }

    goReturn :: InstrReturn -> m InstrReturn
    goReturn = overM instrReturnValue goValue

    goIf :: InstrIf -> m InstrIf
    goIf = overM instrIfArg1 goValue >=> overM instrIfArg2 goValue

    goBranch :: InstrBranch -> m InstrBranch
    goBranch = overM instrBranchValue goValue

    goCase :: InstrCase -> m InstrCase
    goCase = overM instrCaseValue goValue

    goTrace :: InstrTrace -> m InstrTrace
    goTrace = overM instrTraceValue goValue

    goFailure :: InstrFailure -> m InstrFailure
    goFailure = overM instrFailureValue goValue

    goPrealloc :: InstrPrealloc -> m InstrPrealloc
    goPrealloc x = return x

    goBlock :: InstrBlock -> m InstrBlock
    goBlock x = return x

overValueRefs' :: (VarRef -> Value) -> Instruction -> Instruction
overValueRefs' f = runIdentity . overValueRefs'' (return . f)

overValueRefs :: (VarRef -> VarRef) -> Instruction -> Instruction
overValueRefs f = overValueRefs' (VRef . f)

getValueRefs :: Instruction -> [VarRef]
getValueRefs =
  run . execOutputList . overValueRefs'' (\vr -> output vr >> return (VRef vr))

updateLiveVars' :: (VarRef -> Maybe VarRef) -> Instruction -> Instruction
updateLiveVars' f = \case
  Prealloc x -> Prealloc $ over instrPreallocLiveVars (mapMaybe f) x
  Call x -> Call $ over instrCallLiveVars (mapMaybe f) x
  CallClosures x -> CallClosures $ over instrCallClosuresLiveVars (mapMaybe f) x
  instr -> instr

updateLiveVars :: (VarRef -> VarRef) -> Instruction -> Instruction
updateLiveVars f = updateLiveVars' (Just . f)

updateInstrLiveVars :: Instruction -> HashSet VarRef -> HashSet VarRef
updateInstrLiveVars instr liveVars =
  HashSet.union
    (maybe liveVars (`HashSet.delete` liveVars) (getResultVar instr))
    (HashSet.fromList (getValueRefs instr))

computeBackwardLiveVars :: Instruction -> HashSet VarRef -> [HashSet VarRef] -> HashSet VarRef
computeBackwardLiveVars instr live lives = case instr of
  If {} -> ulives
  Branch {} -> ulives
  Case {} -> ulives
  _ -> live
  where
    ulives = HashSet.unions lives
