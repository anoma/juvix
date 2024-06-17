module Juvix.Compiler.Reg.Extra.Base where

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

overValueRefs' :: (VarRef -> Value) -> Instruction -> Instruction
overValueRefs' f = \case
  Binop x -> Binop $ goBinop x
  Unop x -> Unop $ goUnop x
  Cairo x -> Cairo $ goCairo x
  Assign x -> Assign $ goAssign x
  Alloc x -> Alloc $ goAlloc x
  AllocClosure x -> AllocClosure $ goAllocClosure x
  ExtendClosure x -> ExtendClosure $ goExtendClosure x
  Call x -> Call $ goCall x
  CallClosures x -> CallClosures $ goCallClosures x
  TailCall x -> TailCall $ goTailCall x
  TailCallClosures x -> TailCallClosures $ goTailCallClosures x
  Return x -> Return $ goReturn x
  Branch x -> Branch $ goBranch x
  Case x -> Case $ goCase x
  Trace x -> Trace $ goTrace x
  Dump -> Dump
  Failure x -> Failure $ goFailure x
  Prealloc x -> Prealloc $ goPrealloc x
  Nop -> Nop
  Block x -> Block $ goBlock x
  where
    fromVarRef :: Value -> VarRef
    fromVarRef = \case
      VRef r -> r
      _ -> impossible

    goConstrField :: ConstrField -> ConstrField
    goConstrField = over constrFieldRef (fromVarRef . f)

    goValue :: Value -> Value
    goValue = \case
      ValConst c -> ValConst c
      CRef x -> CRef $ goConstrField x
      VRef x -> f x

    goBinop :: InstrBinop -> InstrBinop
    goBinop InstrBinop {..} =
      InstrBinop
        { _instrBinopArg1 = goValue _instrBinopArg1,
          _instrBinopArg2 = goValue _instrBinopArg2,
          ..
        }

    goUnop :: InstrUnop -> InstrUnop
    goUnop = over instrUnopArg goValue

    goCairo :: InstrCairo -> InstrCairo
    goCairo = over instrCairoArgs (map goValue)

    goAssign :: InstrAssign -> InstrAssign
    goAssign = over instrAssignValue goValue

    goAlloc :: InstrAlloc -> InstrAlloc
    goAlloc = over instrAllocArgs (map goValue)

    goAllocClosure :: InstrAllocClosure -> InstrAllocClosure
    goAllocClosure = over instrAllocClosureArgs (map goValue)

    goExtendClosure :: InstrExtendClosure -> InstrExtendClosure
    goExtendClosure InstrExtendClosure {..} =
      InstrExtendClosure
        { _instrExtendClosureValue = fromVarRef (f _instrExtendClosureValue),
          _instrExtendClosureArgs = map goValue _instrExtendClosureArgs,
          ..
        }

    goCallType :: CallType -> CallType
    goCallType = \case
      CallFun sym -> CallFun sym
      CallClosure cl -> CallClosure (fromVarRef (f cl))

    goCall :: InstrCall -> InstrCall
    goCall InstrCall {..} =
      InstrCall
        { _instrCallType = goCallType _instrCallType,
          _instrCallArgs = map goValue _instrCallArgs,
          ..
        }

    goCallClosures :: InstrCallClosures -> InstrCallClosures
    goCallClosures InstrCallClosures {..} =
      InstrCallClosures
        { _instrCallClosuresArgs = map goValue _instrCallClosuresArgs,
          _instrCallClosuresValue = fromVarRef (f _instrCallClosuresValue),
          ..
        }

    goTailCall :: InstrTailCall -> InstrTailCall
    goTailCall InstrTailCall {..} =
      InstrTailCall
        { _instrTailCallType = goCallType _instrTailCallType,
          _instrTailCallArgs = map goValue _instrTailCallArgs,
          ..
        }

    goTailCallClosures :: InstrTailCallClosures -> InstrTailCallClosures
    goTailCallClosures InstrTailCallClosures {..} =
      InstrTailCallClosures
        { _instrTailCallClosuresValue = fromVarRef (f _instrTailCallClosuresValue),
          _instrTailCallClosuresArgs = map goValue _instrTailCallClosuresArgs,
          ..
        }

    goReturn :: InstrReturn -> InstrReturn
    goReturn = over instrReturnValue goValue

    goBranch :: InstrBranch -> InstrBranch
    goBranch = over instrBranchValue goValue

    goCase :: InstrCase -> InstrCase
    goCase = over instrCaseValue goValue

    goTrace :: InstrTrace -> InstrTrace
    goTrace = over instrTraceValue goValue

    goFailure :: InstrFailure -> InstrFailure
    goFailure = over instrFailureValue goValue

    goPrealloc :: InstrPrealloc -> InstrPrealloc
    goPrealloc x = x

    goBlock :: InstrBlock -> InstrBlock
    goBlock x = x

overValueRefs :: (VarRef -> VarRef) -> Instruction -> Instruction
overValueRefs f = overValueRefs' (VRef . f)

updateLiveVars' :: (VarRef -> Maybe VarRef) -> Instruction -> Instruction
updateLiveVars' f = \case
  Prealloc x -> Prealloc $ over instrPreallocLiveVars (mapMaybe f) x
  Call x -> Call $ over instrCallLiveVars (mapMaybe f) x
  CallClosures x -> CallClosures $ over instrCallClosuresLiveVars (mapMaybe f) x
  instr -> instr

updateLiveVars :: (VarRef -> VarRef) -> Instruction -> Instruction
updateLiveVars f = updateLiveVars' (Just . f)
