module Juvix.Compiler.Reg.Extra.Base where

import Juvix.Compiler.Reg.Language

getResultVar :: Instruction -> Maybe VarRef
getResultVar = \case
  Binop x -> Just $ x ^. binaryOpResult
  Show x -> Just $ x ^. instrShowResult
  StrToInt x -> Just $ x ^. instrStrToIntResult
  Assign x -> Just $ x ^. instrAssignResult
  ArgsNum x -> Just $ x ^. instrArgsNumResult
  Alloc x -> Just $ x ^. instrAllocResult
  AllocClosure x -> Just $ x ^. instrAllocClosureResult
  ExtendClosure x -> Just $ x ^. instrExtendClosureResult
  Call x -> Just $ x ^. instrCallResult
  CallClosures x -> Just $ x ^. instrCallClosuresResult
  _ -> Nothing

setResultVar :: Instruction -> VarRef -> Instruction
setResultVar instr vref = case instr of
  Binop x -> Binop $ set binaryOpResult vref x
  Show x -> Show $ set instrShowResult vref x
  StrToInt x -> StrToInt $ set instrStrToIntResult vref x
  Assign x -> Assign $ set instrAssignResult vref x
  ArgsNum x -> ArgsNum $ set instrArgsNumResult vref x
  Alloc x -> Alloc $ set instrAllocResult vref x
  AllocClosure x -> AllocClosure $ set instrAllocClosureResult vref x
  ExtendClosure x -> ExtendClosure $ set instrExtendClosureResult vref x
  Call x -> Call $ set instrCallResult vref x
  CallClosures x -> CallClosures $ set instrCallClosuresResult vref x
  _ -> impossible

overValueRefs :: (VarRef -> VarRef) -> Instruction -> Instruction
overValueRefs f = \case
  Binop x -> Binop $ goBinop x
  Show x -> Show $ goShow x
  StrToInt x -> StrToInt $ goStrToInt x
  Assign x -> Assign $ goAssign x
  ArgsNum x -> ArgsNum $ goArgsNum x
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
    goConstrField :: ConstrField -> ConstrField
    goConstrField = over constrFieldRef f

    goValue :: Value -> Value
    goValue = \case
      Const c -> Const c
      CRef x -> CRef $ goConstrField x
      VRef x -> VRef $ f x

    goBinop :: BinaryOp -> BinaryOp
    goBinop BinaryOp {..} =
      BinaryOp
        { _binaryOpArg1 = goValue _binaryOpArg1,
          _binaryOpArg2 = goValue _binaryOpArg2,
          ..
        }

    goShow :: InstrShow -> InstrShow
    goShow = over instrShowValue goValue

    goStrToInt :: InstrStrToInt -> InstrStrToInt
    goStrToInt = over instrStrToIntValue goValue

    goAssign :: InstrAssign -> InstrAssign
    goAssign = over instrAssignValue goValue

    goArgsNum :: InstrArgsNum -> InstrArgsNum
    goArgsNum = over instrArgsNumValue goValue

    goAlloc :: InstrAlloc -> InstrAlloc
    goAlloc = over instrAllocArgs (map goValue)

    goAllocClosure :: InstrAllocClosure -> InstrAllocClosure
    goAllocClosure = over instrAllocClosureArgs (map goValue)

    goExtendClosure :: InstrExtendClosure -> InstrExtendClosure
    goExtendClosure InstrExtendClosure {..} =
      InstrExtendClosure
        { _instrExtendClosureValue = f _instrExtendClosureValue,
          _instrExtendClosureArgs = map goValue _instrExtendClosureArgs,
          ..
        }

    goCallType :: CallType -> CallType
    goCallType = \case
      CallFun sym -> CallFun sym
      CallClosure cl -> CallClosure (f cl)

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
          _instrCallClosuresValue = f _instrCallClosuresValue,
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
        { _instrTailCallClosuresValue = f _instrTailCallClosuresValue,
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
