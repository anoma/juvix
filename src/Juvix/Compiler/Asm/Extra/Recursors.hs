module Juvix.Compiler.Asm.Extra.Recursors
  ( module Juvix.Compiler.Asm.Extra.Recursors,
    Arguments,
  )
where

import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Error
import Juvix.Compiler.Asm.Extra.Base
import Juvix.Compiler.Asm.Extra.Memory
import Juvix.Compiler.Asm.Extra.Type
import Juvix.Compiler.Asm.Language
import Juvix.Compiler.Asm.Language.Type

-- | Recursor signature. Contains read-only recursor parameters.
data RecursorSig r a = RecursorSig
  { _recursorInfoTable :: InfoTable,
    _recurseInstr :: Memory -> CmdInstr -> Sem r a,
    _recurseBranch :: Memory -> CmdBranch -> [a] -> [a] -> Sem r a,
    _recurseCase :: Memory -> CmdCase -> [[a]] -> Maybe [a] -> Sem r a
  }

makeLenses ''RecursorSig

recurse :: Member (Error AsmError) r => RecursorSig r a -> Arguments -> Code -> Sem r [a]
recurse sig args = fmap snd . recurse' sig (mkMemory args)

recurse' :: forall r a. Member (Error AsmError) r => RecursorSig r a -> Memory -> Code -> Sem r (Memory, [a])
recurse' sig = go
  where
    go :: Memory -> Code -> Sem r (Memory, [a])
    go mem = \case
      [] -> return (mem, [])
      h : t -> case h of
        Instr x -> do
          checkNextInstr (x ^. (cmdInstrInfo . commandInfoLocation)) (x ^. cmdInstrInstruction) t
          goNextCmd (goInstr mem x) t
        Branch x ->
          goNextCmd (goBranch mem x) t
        Case x ->
          goNextCmd (goCase mem x) t

    goNextCmd :: Sem r (Memory, a) -> Code -> Sem r (Memory, [a])
    goNextCmd mp t = do
      (mem', r) <- mp
      (mem'', rs) <- go mem' t
      return (mem'', r : rs)

    checkNextInstr :: Maybe Location -> Instruction -> Code -> Sem r ()
    checkNextInstr loc instr code =
      if
          | isFinalInstr instr && not (null code) ->
              throw $ AsmError loc "unreachable code"
          | otherwise ->
              return ()

    goInstr :: Memory -> CmdInstr -> Sem r (Memory, a)
    goInstr memory cmd = do
      a <- (sig ^. recurseInstr) memory cmd
      mem <- fixMemInstr memory (cmd ^. cmdInstrInstruction)
      return (mem, a)
      where
        loc = cmd ^. (cmdInstrInfo . commandInfoLocation)

        fixMemInstr :: Memory -> Instruction -> Sem r Memory
        fixMemInstr mem instr =
          case instr of
            IntAdd ->
              fixMemIntOp mem
            IntSub ->
              fixMemIntOp mem
            IntMul ->
              fixMemIntOp mem
            IntDiv ->
              fixMemIntOp mem
            IntLt ->
              fixMemBinOp mem mkInteger mkInteger mkBool
            IntLe ->
              fixMemBinOp mem mkInteger mkInteger mkBool
            ValEq ->
              fixMemBinOp mem TyDynamic TyDynamic mkBool
            Push val -> do
              ty <- getValueType' loc mem val
              return (pushValueStack ty mem)
            Pop -> do
              when (null (mem ^. memoryValueStack)) $
                throw $
                  AsmError loc "popping empty value stack"
              return (popValueStack 1 mem)
            PushTemp -> do
              when (null (mem ^. memoryValueStack)) $
                throw $
                  AsmError loc "popping empty value stack"
              return $ pushTempStack (topValueStack' 0 mem) mem
            PopTemp -> do
              when (null (mem ^. memoryTempStack)) $
                throw $
                  AsmError loc "popping empty temporary stack"
              return $ popTempStack 1 mem
            Trace ->
              return mem
            Failure ->
              return mem
            AllocConstr tag -> do
              let ci = getConstrInfo (sig ^. recursorInfoTable) tag
              let n = ci ^. constructorArgsNum
              let tyargs = typeArgs (ci ^. constructorType)
              checkValueStack' loc tyargs mem
              tys <-
                zipWithM
                  (\ty idx -> unifyTypes' loc ty (topValueStack' idx mem))
                  tyargs
                  [0 ..]
              return $
                pushValueStack (mkTypeConstr (ci ^. constructorInductive) tag tys) $
                  popValueStack n mem
            AllocClosure InstrAllocClosure {..} -> do
              let fi = getFunInfo (sig ^. recursorInfoTable) _allocClosureFunSymbol
              let (tyargs, tgt) = unfoldType (fi ^. functionType)
              checkValueStack' loc (take _allocClosureArgsNum tyargs) mem
              return $
                pushValueStack (mkTypeFun (drop _allocClosureArgsNum tyargs) tgt) $
                  popValueStack _allocClosureArgsNum mem
            ExtendClosure x ->
              fixMemExtendClosure mem x
            Call x ->
              fixMemCall mem x
            TailCall x ->
              fixMemCall mem x
            CallClosures x ->
              fixMemCallClosures mem x
            TailCallClosures x ->
              fixMemCallClosures mem x
            Return ->
              return mem

        fixMemIntOp :: Memory -> Sem r Memory
        fixMemIntOp mem = fixMemBinOp mem mkInteger mkInteger mkInteger

        fixMemBinOp :: Memory -> Type -> Type -> Type -> Sem r Memory
        fixMemBinOp mem ty0 ty1 rty = do
          checkValueStack' loc [ty0, ty1] mem
          return $ pushValueStack rty (popValueStack 2 mem)

        fixMemExtendClosure :: Memory -> InstrExtendClosure -> Sem r Memory
        fixMemExtendClosure mem InstrExtendClosure {..} = do
          when (length (mem ^. memoryValueStack) < _extendClosureArgsNum + 1) $
            throw $
              AsmError loc "invalid closure extension: not enough values on the stack"
          let ty = topValueStack' 0 mem
          checkFunType ty
          when (ty /= TyDynamic && length (typeArgs ty) < _extendClosureArgsNum) $
            throw $
              AsmError loc "invalid closure extension: too many supplied arguments"
          fixMemValueStackArgs mem 1 _extendClosureArgsNum ty

        fixMemCall :: Memory -> InstrCall -> Sem r Memory
        fixMemCall mem InstrCall {..} = do
          let k = if _callType == CallClosure then 1 else 0
          when (length (mem ^. memoryValueStack) < _callArgsNum + k) $
            throw $
              AsmError loc "invalid call: not enough values on the stack"
          let ty = case _callType of
                CallClosure -> topValueStack' 0 mem
                CallFun sym -> getFunInfo (sig ^. recursorInfoTable) sym ^. functionType
          checkFunType ty
          when (ty /= TyDynamic && length (typeArgs ty) /= _callArgsNum) $
            throw $
              AsmError loc "invalid call: the number of supplied arguments doesn't match the number of expected arguments"
          fixMemValueStackArgs mem k _callArgsNum ty

        fixMemCallClosures :: Memory -> InstrCallClosures -> Sem r Memory
        fixMemCallClosures mem InstrCallClosures {..} = do
          when (null (mem ^. memoryValueStack)) $
            throw $
              AsmError loc "invalid closure call: value stack is empty"
          let ty = topValueStack' 0 mem
          checkFunType ty
          if
              | ty == TyDynamic -> do
                  let mem' = popValueStack 1 mem
                  return $ pushValueStack TyDynamic (popValueStack _callClosuresArgsNum mem')
              | length (typeArgs ty) < _callClosuresArgsNum -> do
                  let n = length (typeArgs ty)
                  mem' <- fixMemCall mem (InstrCall CallClosure n)
                  fixMemCallClosures mem' (InstrCallClosures (_callClosuresArgsNum - n))
              | length (typeArgs ty) > _callClosuresArgsNum -> do
                  let n = length (typeArgs ty)
                  mem' <- fixMemExtendClosure mem (InstrExtendClosure n)
                  fixMemCallClosures mem' (InstrCallClosures (_callClosuresArgsNum - n))
              | otherwise ->
                  fixMemCall mem (InstrCall CallClosure (length (typeArgs ty)))

        fixMemValueStackArgs :: Memory -> Int -> Int -> Type -> Sem r Memory
        fixMemValueStackArgs mem k argsNum ty = do
          let mem' = popValueStack k mem
          let tyargs = topValuesFromValueStack' argsNum mem'
          -- `typeArgs ty` may be shorter than `tyargs` only if `ty` is dynamic
          zipWithM_ (unifyTypes' loc) tyargs (typeArgs ty)
          return $
            pushValueStack (mkTypeFun (drop argsNum (typeArgs ty)) ty) $
              popValueStack argsNum mem'

        checkFunType :: Type -> Sem r ()
        checkFunType ty = void $ unifyTypes' loc ty (mkTypeFun [TyDynamic] TyDynamic)

    goBranch :: Memory -> CmdBranch -> Sem r (Memory, a)
    goBranch mem cmd@CmdBranch {..} = do
      (mem1, as1) <- go mem _cmdBranchTrue
      (mem2, as2) <- go mem _cmdBranchFalse
      a' <- (sig ^. recurseBranch) mem cmd as1 as2
      mem' <- unifyMemory' loc mem1 mem2
      checkBranchInvariant loc mem mem'
      return (mem', a')
      where
        loc = cmd ^. (cmdBranchInfo . commandInfoLocation)

    goCase :: Memory -> CmdCase -> Sem r (Memory, a)
    goCase mem cmd@CmdCase {..} = do
      rs <- mapM (go mem . (^. caseBranchCode)) _cmdCaseBranches
      let mems = map fst rs
      let ass = map snd rs
      rd <- maybe (return Nothing) (fmap Just . go mem) _cmdCaseDefault
      let md = fmap fst rd
      let ad = fmap snd rd
      a' <- (sig ^. recurseCase) mem cmd ass ad
      mem' <- foldr (\m rm -> rm >>= unifyMemory' loc m) (return mem) mems
      mem'' <- maybe (return mem') (unifyMemory' loc mem') md
      checkBranchInvariant loc mem mem''
      return (mem'', a')
      where
        loc = cmd ^. (cmdCaseInfo . commandInfoLocation)

    checkBranchInvariant :: Maybe Location -> Memory -> Memory -> Sem r ()
    checkBranchInvariant loc mem mem' = do
      unless
        ( length (mem' ^. memoryValueStack) == length (mem ^. memoryValueStack)
            || length (mem' ^. memoryValueStack) == length (mem ^. memoryValueStack) + 1
        )
        $ throw
        $ AsmError loc "wrong value stack height after branching (can increase by at most 1)"
      unless (length (mem' ^. memoryTempStack) == length (mem ^. memoryTempStack)) $
        throw $
          AsmError loc "temporary stack height changed after branching"
