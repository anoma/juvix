module Juvix.Compiler.Asm.Extra.Recursors
  ( module Juvix.Compiler.Asm.Extra.Recursors,
    module Juvix.Compiler.Asm.Extra.Memory,
  )
where

import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Data.Stack qualified as Stack
import Juvix.Compiler.Asm.Error
import Juvix.Compiler.Asm.Extra.Base
import Juvix.Compiler.Asm.Extra.Memory
import Juvix.Compiler.Asm.Extra.Type
import Juvix.Compiler.Asm.Language
import Juvix.Compiler.Asm.Pretty

-- | Recursor signature. Contains read-only recursor parameters.
data RecursorSig m r a = RecursorSig
  { _recursorInfoTable :: InfoTable,
    _recurseInstr :: m -> CmdInstr -> Sem r a,
    _recurseBranch :: m -> CmdBranch -> [a] -> [a] -> Sem r a,
    _recurseCase :: m -> CmdCase -> [[a]] -> Maybe [a] -> Sem r a
  }

makeLenses ''RecursorSig

recurseFun :: (Member (Error AsmError) r) => RecursorSig Memory r a -> FunctionInfo -> Sem r [a]
recurseFun sig fi = recurse sig (argumentsFromFunctionInfo fi) (fi ^. functionCode)

recurse :: (Member (Error AsmError) r) => RecursorSig Memory r a -> Arguments -> Code -> Sem r [a]
recurse sig args = fmap snd . recurse' sig (mkMemory args)

recurse' :: forall r a. (Member (Error AsmError) r) => RecursorSig Memory r a -> Memory -> Code -> Sem r (Memory, [a])
recurse' sig = go True
  where
    go :: Bool -> Memory -> Code -> Sem r (Memory, [a])
    go isTail mem = \case
      [] -> return (mem, [])
      h : t -> case h of
        Instr x -> do
          checkNextInstr isTail (x ^. (cmdInstrInfo . commandInfoLocation)) (x ^. cmdInstrInstruction) t
          goNextCmd isTail (x ^. (cmdInstrInfo . commandInfoLocation)) (goInstr mem x) t
        Branch x ->
          goNextCmd isTail (x ^. (cmdBranchInfo . commandInfoLocation)) (goBranch (isTail && null t) mem x) t
        Case x ->
          goNextCmd isTail (x ^. (cmdCaseInfo . commandInfoLocation)) (goCase (isTail && null t) mem x) t

    goNextCmd :: Bool -> Maybe Location -> Sem r (Memory, a) -> Code -> Sem r (Memory, [a])
    goNextCmd isTail loc mp t = do
      (mem', r) <- mp
      when (isTail && null t && length (mem' ^. memoryValueStack) /= 1) $
        throw $
          AsmError loc "expected value stack height 1 on function exit"
      (mem'', rs) <- go isTail mem' t
      return (mem'', r : rs)

    checkNextInstr :: Bool -> Maybe Location -> Instruction -> Code -> Sem r ()
    checkNextInstr isTail loc instr code
      | isFinalInstr instr && not (null code) =
          throw $ AsmError loc "unreachable code"
      | isTail && null code && not (isFinalInstr instr) =
          throw $ AsmError loc "expected a return or a tail call"
      | otherwise =
          return ()

    goInstr :: Memory -> CmdInstr -> Sem r (Memory, a)
    goInstr memory cmd = do
      a <- (sig ^. recurseInstr) memory cmd
      mem <- fixMemInstr memory (cmd ^. cmdInstrInstruction)
      return (mem, a)
      where
        loc = cmd ^. cmdInstrInfo . commandInfoLocation

        fixMemInstr :: Memory -> Instruction -> Sem r Memory
        fixMemInstr mem instr =
          case instr of
            Binop IntAdd ->
              fixMemIntOp mem
            Binop IntSub ->
              fixMemIntOp mem
            Binop IntMul ->
              fixMemIntOp mem
            Binop IntDiv ->
              fixMemIntOp mem
            Binop IntMod ->
              fixMemIntOp mem
            Binop IntLt ->
              fixMemBinOp mem mkTypeInteger mkTypeInteger mkTypeBool
            Binop IntLe ->
              fixMemBinOp mem mkTypeInteger mkTypeInteger mkTypeBool
            Binop ValEq ->
              fixMemBinOp mem TyDynamic TyDynamic mkTypeBool
            Binop StrConcat ->
              fixMemBinOp mem TyString TyString TyString
            ValShow ->
              return (pushValueStack TyString (popValueStack 1 mem))
            StrToInt -> do
              checkValueStack' loc (sig ^. recursorInfoTable) [TyString] mem
              return (pushValueStack mkTypeInteger (popValueStack 1 mem))
            Push val -> do
              ty <- getValueType' loc (sig ^. recursorInfoTable) mem val
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
              return $ pushTempStack (topValueStack' 0 mem) (popValueStack 1 mem)
            PopTemp -> do
              when (null (mem ^. memoryTempStack)) $
                throw $
                  AsmError loc "popping empty temporary stack"
              return $ popTempStack 1 mem
            Trace ->
              return mem
            Dump ->
              return mem
            Failure ->
              return mem
            Prealloc {} ->
              return mem
            AllocConstr tag -> do
              let ci = getConstrInfo (sig ^. recursorInfoTable) tag
                  n = ci ^. constructorArgsNum
                  tyargs = typeArgs (ci ^. constructorType)
              checkValueStack' loc (sig ^. recursorInfoTable) tyargs mem
              tys <-
                zipWithM
                  (\ty idx -> unifyTypes' loc (sig ^. recursorInfoTable) ty (topValueStack' idx mem))
                  tyargs
                  [0 ..]
              return $
                pushValueStack (mkTypeConstr (ci ^. constructorInductive) tag tys) $
                  popValueStack n mem
            AllocClosure InstrAllocClosure {..} -> do
              let fi = getFunInfo (sig ^. recursorInfoTable) _allocClosureFunSymbol
                  (tyargs, tgt) = unfoldType (fi ^. functionType)
              checkValueStack' loc (sig ^. recursorInfoTable) (take _allocClosureArgsNum tyargs) mem
              return $
                pushValueStack (mkTypeFun (drop _allocClosureArgsNum tyargs) tgt) $
                  popValueStack _allocClosureArgsNum mem
            ExtendClosure x ->
              fixMemExtendClosure mem x
            Call x ->
              fixMemCall mem x
            TailCall x ->
              fixMemCall (dropTempStack mem) x
            CallClosures x ->
              fixMemCallClosures mem x
            TailCallClosures x ->
              fixMemCallClosures (dropTempStack mem) x
            Return ->
              return (dropTempStack mem)

        fixMemIntOp :: Memory -> Sem r Memory
        fixMemIntOp mem = fixMemBinOp mem mkTypeInteger mkTypeInteger mkTypeInteger

        fixMemBinOp :: Memory -> Type -> Type -> Type -> Sem r Memory
        fixMemBinOp mem ty0 ty1 rty = do
          checkValueStack' loc (sig ^. recursorInfoTable) [ty0, ty1] mem
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
          let argsNum = case _callType of
                CallClosure -> length (typeArgs ty)
                CallFun sym -> getFunInfo (sig ^. recursorInfoTable) sym ^. functionArgsNum
          checkFunType ty
          when (ty /= TyDynamic && argsNum /= _callArgsNum) $
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
              | length (typeArgs ty) > _callClosuresArgsNum ->
                  fixMemExtendClosure mem (InstrExtendClosure _callClosuresArgsNum)
              | otherwise ->
                  fixMemCall mem (InstrCall CallClosure (length (typeArgs ty)))

        fixMemValueStackArgs :: Memory -> Int -> Int -> Type -> Sem r Memory
        fixMemValueStackArgs mem k argsNum ty = do
          checkValueStackHeight' loc (argsNum + k) mem
          let mem' = popValueStack k mem
          unless (ty == TyDynamic) $
            checkValueStack' loc (sig ^. recursorInfoTable) (take argsNum (typeArgs ty)) mem'
          let tyargs = topValuesFromValueStack' argsNum mem'
          -- `typeArgs ty` may be shorter than `tyargs` only if `ty` is dynamic
          zipWithM_ (unifyTypes' loc (sig ^. recursorInfoTable)) tyargs (typeArgs ty)
          return $
            pushValueStack (mkTypeFun (drop argsNum (typeArgs ty)) (typeTarget ty)) $
              popValueStack argsNum mem'

        dropTempStack :: Memory -> Memory
        dropTempStack mem = mem {_memoryTempStack = Stack.empty}

        checkFunType :: Type -> Sem r ()
        checkFunType = \case
          TyDynamic -> return ()
          TyFun {} -> return ()
          ty ->
            throw $
              AsmError
                loc
                ( "expected a function, got value of type "
                    <> ppTrace (sig ^. recursorInfoTable) ty
                )

    goBranch :: Bool -> Memory -> CmdBranch -> Sem r (Memory, a)
    goBranch isTail mem cmd@CmdBranch {..} = do
      checkValueStack' loc (sig ^. recursorInfoTable) [mkTypeBool] mem
      let mem0 = popValueStack 1 mem
      (mem1, as1) <- go isTail mem0 _cmdBranchTrue
      (mem2, as2) <- go isTail mem0 _cmdBranchFalse
      a' <- (sig ^. recurseBranch) mem cmd as1 as2
      mem' <- unifyMemory' loc (sig ^. recursorInfoTable) mem1 mem2
      checkBranchInvariant 1 loc mem0 mem'
      return (mem', a')
      where
        loc = cmd ^. cmdBranchInfo . commandInfoLocation

    goCase :: Bool -> Memory -> CmdCase -> Sem r (Memory, a)
    goCase isTail mem cmd@CmdCase {..} = do
      checkValueStack' loc (sig ^. recursorInfoTable) [mkTypeInductive _cmdCaseInductive] mem
      rs <- mapM (go isTail mem . (^. caseBranchCode)) _cmdCaseBranches
      let mems = map fst rs
          ass = map snd rs
      rd <- maybe (return Nothing) (fmap Just . go isTail mem) _cmdCaseDefault
      let md = fmap fst rd
          ad = fmap snd rd
      a' <- (sig ^. recurseCase) mem cmd ass ad
      case mems of
        [] -> return (fromMaybe mem md, a')
        mem0 : mems' -> do
          mem' <- foldr (\m rm -> rm >>= unifyMemory' loc (sig ^. recursorInfoTable) m) (return mem0) mems'
          mem'' <- maybe (return mem') (unifyMemory' loc (sig ^. recursorInfoTable) mem') md
          checkBranchInvariant 0 loc mem mem''
          return (mem'', a')
      where
        loc = cmd ^. (cmdCaseInfo . commandInfoLocation)

    checkBranchInvariant :: Int -> Maybe Location -> Memory -> Memory -> Sem r ()
    checkBranchInvariant k loc mem mem' = do
      unless (length (mem' ^. memoryValueStack) == length (mem ^. memoryValueStack) + k) $
        throw $
          AsmError loc ("wrong value stack height after branching (must increase by " <> show k <> ")")
      unless
        ( null (mem' ^. memoryTempStack)
            || length (mem' ^. memoryTempStack) == length (mem ^. memoryTempStack)
        )
        $ throw
        $ AsmError loc "temporary stack height changed after branching"

data StackInfo = StackInfo
  { _stackInfoValueStackHeight :: Int,
    _stackInfoTempStackHeight :: Int
  }
  deriving stock (Eq)

makeLenses ''StackInfo

initialStackInfo :: StackInfo
initialStackInfo = StackInfo {_stackInfoValueStackHeight = 0, _stackInfoTempStackHeight = 0}

-- | A simplified recursor which doesn't perform validity checking and only
-- computes stack height information. This makes a significant performance
-- difference, since we'll have many passes recursing over the entire JuvixAsm
-- program code which need only stack height information. Also, the code using
-- the simplified recursor can itself be simpler if it doesn't need the extra
-- info provided by the full recursor.
recurseS :: forall r a. (Member (Error AsmError) r) => RecursorSig StackInfo r a -> Code -> Sem r [a]
recurseS sig code = snd <$> recurseS' sig initialStackInfo code

recurseS' :: forall r a. (Member (Error AsmError) r) => RecursorSig StackInfo r a -> StackInfo -> Code -> Sem r (StackInfo, [a])
recurseS' sig = go
  where
    go :: StackInfo -> Code -> Sem r (StackInfo, [a])
    go si = \case
      [] -> return (si, [])
      h : t -> case h of
        Instr x -> do
          goNextCmd (goInstr si x) t
        Branch x ->
          goNextCmd (goBranch si x) t
        Case x ->
          goNextCmd (goCase si x) t

    goNextCmd :: Sem r (StackInfo, a) -> Code -> Sem r (StackInfo, [a])
    goNextCmd mp t = do
      (si', r) <- mp
      (si'', rs) <- go si' t
      return (si'', r : rs)

    goInstr :: StackInfo -> CmdInstr -> Sem r (StackInfo, a)
    goInstr stackInfo cmd = do
      a <- (sig ^. recurseInstr) stackInfo cmd
      si' <- fixStackInstr stackInfo (cmd ^. cmdInstrInstruction)
      return (si', a)
      where
        fixStackInstr :: StackInfo -> Instruction -> Sem r StackInfo
        fixStackInstr si instr =
          case instr of
            Binop IntAdd ->
              fixStackBinOp si
            Binop IntSub ->
              fixStackBinOp si
            Binop IntMul ->
              fixStackBinOp si
            Binop IntDiv ->
              fixStackBinOp si
            Binop IntMod ->
              fixStackBinOp si
            Binop IntLt ->
              fixStackBinOp si
            Binop IntLe ->
              fixStackBinOp si
            Binop ValEq ->
              fixStackBinOp si
            Binop StrConcat ->
              fixStackBinOp si
            ValShow ->
              return si
            StrToInt ->
              return si
            Push {} -> do
              return (stackInfoPushValueStack 1 si)
            Pop -> do
              return (stackInfoPopValueStack 1 si)
            PushTemp -> do
              return $ stackInfoPushTempStack 1 (stackInfoPopValueStack 1 si)
            PopTemp -> do
              return $ stackInfoPopTempStack 1 si
            Trace ->
              return si
            Dump ->
              return si
            Failure ->
              return si
            Prealloc {} ->
              return si
            AllocConstr tag -> do
              let ci = getConstrInfo (sig ^. recursorInfoTable) tag
                  n = ci ^. constructorArgsNum
              return $
                stackInfoPopValueStack (n - 1) si
            AllocClosure InstrAllocClosure {..} -> do
              return $
                stackInfoPopValueStack (_allocClosureArgsNum - 1) si
            ExtendClosure InstrExtendClosure {..} ->
              return $
                stackInfoPopValueStack _extendClosureArgsNum si
            Call x ->
              fixStackCall si x
            TailCall x ->
              fixStackCall (dropTempStack si) x
            CallClosures x ->
              fixStackCallClosures si x
            TailCallClosures x ->
              fixStackCallClosures (dropTempStack si) x
            Return ->
              return (dropTempStack si)

        fixStackBinOp :: StackInfo -> Sem r StackInfo
        fixStackBinOp si = return $ stackInfoPopValueStack 1 si

        fixStackCall :: StackInfo -> InstrCall -> Sem r StackInfo
        fixStackCall si InstrCall {..} = do
          return $ stackInfoPopValueStack (_callArgsNum + (if _callType == CallClosure then 1 else 0) - 1) si

        fixStackCallClosures :: StackInfo -> InstrCallClosures -> Sem r StackInfo
        fixStackCallClosures si InstrCallClosures {..} = do
          return $ stackInfoPopValueStack _callClosuresArgsNum si

    goBranch :: StackInfo -> CmdBranch -> Sem r (StackInfo, a)
    goBranch si cmd@CmdBranch {..} = do
      let si0 = stackInfoPopValueStack 1 si
      (si1, as1) <- go si0 _cmdBranchTrue
      (si2, as2) <- go si0 _cmdBranchFalse
      a' <- (sig ^. recurseBranch) si cmd as1 as2
      checkStackInfo loc si1 si2
      return (si1, a')
      where
        loc = cmd ^. cmdBranchInfo . commandInfoLocation

    goCase :: StackInfo -> CmdCase -> Sem r (StackInfo, a)
    goCase si cmd@CmdCase {..} = do
      rs <- mapM (go si . (^. caseBranchCode)) _cmdCaseBranches
      let sis = map fst rs
          ass = map snd rs
      rd <- maybe (return Nothing) (fmap Just . go si) _cmdCaseDefault
      let sd = fmap fst rd
          ad = fmap snd rd
      a' <- (sig ^. recurseCase) si cmd ass ad
      case sis of
        [] -> return (fromMaybe si sd, a')
        si0 : sis' -> do
          mapM_ (checkStackInfo loc si0) sis'
          forM_ sd (checkStackInfo loc si0)
          return (si0, a')
      where
        loc = cmd ^. (cmdCaseInfo . commandInfoLocation)

    checkStackInfo :: Maybe Location -> StackInfo -> StackInfo -> Sem r ()
    checkStackInfo loc si1 si2 =
      when (si1 /= si2) $
        throw $
          AsmError loc "stack height mismatch"

    stackInfoPushValueStack :: Int -> StackInfo -> StackInfo
    stackInfoPushValueStack n si = si {_stackInfoValueStackHeight = si ^. stackInfoValueStackHeight + n}

    stackInfoPopValueStack :: Int -> StackInfo -> StackInfo
    stackInfoPopValueStack n si = si {_stackInfoValueStackHeight = si ^. stackInfoValueStackHeight - n}

    stackInfoPushTempStack :: Int -> StackInfo -> StackInfo
    stackInfoPushTempStack n si = si {_stackInfoTempStackHeight = si ^. stackInfoTempStackHeight + n}

    stackInfoPopTempStack :: Int -> StackInfo -> StackInfo
    stackInfoPopTempStack n si = si {_stackInfoTempStackHeight = si ^. stackInfoTempStackHeight - n}

    dropTempStack :: StackInfo -> StackInfo
    dropTempStack si = si {_stackInfoTempStackHeight = 0}

-- | Fold signature. Contains read-only fold parameters.
data FoldSig m r a = FoldSig
  { _foldInfoTable :: InfoTable,
    _foldAdjust :: a -> a,
    _foldInstr :: m -> CmdInstr -> a -> Sem r a,
    _foldBranch :: m -> CmdBranch -> a -> a -> a -> Sem r a,
    _foldCase :: m -> CmdCase -> [a] -> Maybe a -> a -> Sem r a
  }

makeLenses ''FoldSig

foldS :: forall r a. (Member (Error AsmError) r) => FoldSig StackInfo r a -> Code -> a -> Sem r a
foldS sig code a = snd <$> foldS' sig initialStackInfo code a

foldS' :: forall r a. (Member (Error AsmError) r) => FoldSig StackInfo r a -> StackInfo -> Code -> a -> Sem r (StackInfo, a)
foldS' sig si code acc = do
  (si', fs) <- recurseS' sig' si code
  a' <- compose fs acc
  return (si', a')
  where
    sig' :: RecursorSig StackInfo r (a -> Sem r a)
    sig' =
      RecursorSig
        { _recursorInfoTable = sig ^. foldInfoTable,
          _recurseInstr = \s cmd -> return ((sig ^. foldInstr) s cmd),
          _recurseBranch = \s cmd br1 br2 ->
            return
              ( \a -> do
                  let a' = (sig ^. foldAdjust) a
                  a1 <- compose br1 a'
                  a2 <- compose br2 a'
                  (sig ^. foldBranch) s cmd a1 a2 a
              ),
          _recurseCase = \s cmd brs md ->
            return
              ( \a -> do
                  let a' = (sig ^. foldAdjust) a
                  as <- mapM (`compose` a') brs
                  ad <- case md of
                    Just d -> Just <$> compose d a'
                    Nothing -> return Nothing
                  (sig ^. foldCase) s cmd as ad a
              )
        }

    compose :: [a -> Sem r a] -> a -> Sem r a
    compose lst x = foldr (=<<) (return x) lst
