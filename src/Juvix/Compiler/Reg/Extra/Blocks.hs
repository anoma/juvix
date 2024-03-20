module Juvix.Compiler.Reg.Extra.Blocks where

import Juvix.Compiler.Reg.Language.Blocks

overBlocks :: (Block -> Block) -> Block -> Block
overBlocks f block = block'
  where
    block' = over blockFinal (fmap goFinal) $ over blockNext (fmap f) block

    goFinal :: FinalInstruction -> FinalInstruction
    goFinal = \case
      Call x -> Call x
      TailCall x -> TailCall x
      Return x -> Return x
      Branch x ->
        Branch $ over instrBranchTrue f $ over instrBranchFalse f x
      Case x ->
        Case $
          over instrCaseDefault (fmap f) $
            over instrCaseBranches (map (over caseBranchCode f)) x

getBlocks :: Block -> [Block]
getBlocks block =
  maybeToList (block ^. blockNext) ++ concatMap goFinal (maybeToList (block ^. blockFinal))
  where
    goFinal :: FinalInstruction -> [Block]
    goFinal = \case
      Call {} -> []
      TailCall {} -> []
      Return {} -> []
      Branch x ->
        [x ^. instrBranchTrue, x ^. instrBranchFalse]
      Case x ->
        maybeToList (x ^. instrCaseDefault)
          ++ map (^. caseBranchCode) (x ^. instrCaseBranches)

getResultVar :: Instruction -> Maybe VarRef
getResultVar = \case
  Binop x -> Just $ x ^. instrBinopResult
  Unop x -> Just $ x ^. instrUnopResult
  Assign x -> Just $ x ^. instrAssignResult
  Alloc x -> Just $ x ^. instrAllocResult
  AllocClosure x -> Just $ x ^. instrAllocClosureResult
  ExtendClosure x -> Just $ x ^. instrExtendClosureResult
  _ -> Nothing

getResultVar' :: FinalInstruction -> Maybe VarRef
getResultVar' = \case
  Call x -> Just $ x ^. instrCallResult
  _ -> Nothing

getValueRefs'' :: Value -> [VarRef]
getValueRefs'' = \case
  Const {} -> []
  CRef ConstrField {..} -> [_constrFieldRef]
  VRef x -> [x]

getValueRefs :: Instruction -> [VarRef]
getValueRefs = \case
  Binop x -> goBinop x
  Unop x -> goUnop x
  Assign x -> goAssign x
  Alloc x -> goAlloc x
  AllocClosure x -> goAllocClosure x
  ExtendClosure x -> goExtendClosure x
  Trace x -> goTrace x
  Dump -> []
  Failure x -> goFailure x
  where
    goBinop :: InstrBinop -> [VarRef]
    goBinop InstrBinop {..} =
      getValueRefs'' _instrBinopArg1
        ++ getValueRefs'' _instrBinopArg2

    goUnop :: InstrUnop -> [VarRef]
    goUnop InstrUnop {..} = getValueRefs'' _instrUnopArg

    goAssign :: InstrAssign -> [VarRef]
    goAssign InstrAssign {..} = getValueRefs'' _instrAssignValue

    goAlloc :: InstrAlloc -> [VarRef]
    goAlloc InstrAlloc {..} = concatMap getValueRefs'' _instrAllocArgs

    goAllocClosure :: InstrAllocClosure -> [VarRef]
    goAllocClosure InstrAllocClosure {..} = concatMap getValueRefs'' _instrAllocClosureArgs

    goExtendClosure :: InstrExtendClosure -> [VarRef]
    goExtendClosure InstrExtendClosure {..} =
      _instrExtendClosureValue : concatMap getValueRefs'' _instrExtendClosureArgs

    goTrace :: InstrTrace -> [VarRef]
    goTrace InstrTrace {..} = getValueRefs'' _instrTraceValue

    goFailure :: InstrFailure -> [VarRef]
    goFailure InstrFailure {..} = getValueRefs'' _instrFailureValue

getValueRefs' :: FinalInstruction -> [VarRef]
getValueRefs' = \case
  Call x -> goCall x
  TailCall x -> goTailCall x
  Return x -> goReturn x
  Branch x -> goBranch x
  Case x -> goCase x
  where
    goCallType :: CallType -> [VarRef]
    goCallType = \case
      CallFun {} -> []
      CallClosure cl -> [cl]

    goCall :: InstrCall -> [VarRef]
    goCall InstrCall {..} = goCallType _instrCallType ++ concatMap getValueRefs'' _instrCallArgs

    goTailCall :: InstrTailCall -> [VarRef]
    goTailCall InstrTailCall {..} =
      goCallType _instrTailCallType ++ concatMap getValueRefs'' _instrTailCallArgs

    goReturn :: InstrReturn -> [VarRef]
    goReturn InstrReturn {..} = getValueRefs'' _instrReturnValue

    goBranch :: InstrBranch -> [VarRef]
    goBranch InstrBranch {..} = getValueRefs'' _instrBranchValue

    goCase :: InstrCase -> [VarRef]
    goCase InstrCase {..} = getValueRefs'' _instrCaseValue
