module Juvix.Compiler.Casm.Translation.FromReg where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Backend
import Juvix.Compiler.Casm.Data.LabelInfoBuilder
import Juvix.Compiler.Casm.Data.Limits
import Juvix.Compiler.Casm.Data.Result
import Juvix.Compiler.Casm.Extra.Base
import Juvix.Compiler.Casm.Extra.Stdlib
import Juvix.Compiler.Casm.Language
import Juvix.Compiler.Reg.Data.InfoTable qualified as Reg
import Juvix.Compiler.Reg.Extra.Info qualified as Reg
import Juvix.Compiler.Reg.Language qualified as Reg
import Juvix.Compiler.Tree.Evaluator.Builtins qualified as Reg
import Juvix.Data.Field

fromReg :: Reg.InfoTable -> Result
fromReg tab = uncurry Result $ run $ runLabelInfoBuilderWithNextId (Reg.getNextSymbolId tab) $ do
  let initialOffset :: Int = 2
  (blts, binstrs) <- addStdlibBuiltins initialOffset
  endSym <- freshSymbol
  let endName :: Text = "__juvix_end"
      endLab = LabelRef endSym (Just endName)
  (addr, instrs) <- second (concat . reverse) <$> foldM (goFun blts endLab) (initialOffset + length binstrs, []) (tab ^. Reg.infoFunctions)
  eassert (addr == length instrs + length binstrs + initialOffset)
  registerLabelName endSym endName
  registerLabelAddress endSym addr
  let mainSym = fromJust $ tab ^. Reg.infoMainFunction
      mainName = fromJust (HashMap.lookup mainSym (tab ^. Reg.infoFunctions)) ^. Reg.functionName
      callInstr = Call $ InstrCall $ Lab $ LabelRef mainSym (Just mainName)
      jmpInstr = mkJump (Val $ Lab endLab)
  return $ callInstr : jmpInstr : binstrs ++ instrs ++ [Label endLab]
  where
    info :: Reg.ExtraInfo
    info = Reg.computeExtraInfo (getLimits TargetCairo False) tab

    getTagId :: Tag -> Int
    getTagId tag =
      1 + 2 * fromJust (HashMap.lookup tag (info ^. Reg.extraInfoCIDs))

    goFun :: forall r. (Member LabelInfoBuilder r) => StdlibBuiltins -> LabelRef -> (Address, [[Instruction]]) -> Reg.FunctionInfo -> Sem r (Address, [[Instruction]])
    goFun blts failLab (addr0, acc) funInfo = do
      let sym = funInfo ^. Reg.functionSymbol
          funName = funInfo ^. Reg.functionName
      registerLabelName sym funName
      registerLabelAddress sym addr0
      let lab = Label $ LabelRef sym (Just funName)
          code = funInfo ^. Reg.functionCode
          n = fromJust $ HashMap.lookup (funInfo ^. Reg.functionSymbol) (info ^. Reg.extraInfoLocalVarsNum)
          i1 = Alloc $ InstrAlloc $ Val $ Imm $ fromIntegral n
          pre = [lab, i1]
          addr1 = addr0 + length pre
      instrs <- goCode addr1 code
      return (addr1 + length instrs, (pre ++ instrs) : acc)
      where
        unsupported :: Text -> a
        unsupported what = error ("Cairo backend: unsupported: " <> what)

        goCode :: Address -> Reg.Code -> Sem r [Instruction]
        goCode addr code = concat . reverse . snd <$> foldM go' (addr, []) code
          where
            go' :: (Address, [[Instruction]]) -> Reg.Instruction -> Sem r (Address, [[Instruction]])
            go' (addr', acc') i = do
              is <- goInstr addr' i
              return (addr' + length is, is : acc')

        goInstr :: Address -> Reg.Instruction -> Sem r [Instruction]
        goInstr addr = \case
          Reg.Binop x -> goBinop addr x
          Reg.Unop x -> goUnop addr x
          Reg.Assign x -> goAssign addr x
          Reg.Alloc x -> goAlloc addr x
          Reg.AllocClosure x -> goAllocClosure addr x
          Reg.ExtendClosure x -> goExtendClosure addr x
          Reg.Call x -> goCall addr x
          Reg.TailCall x -> goTailCall addr x
          Reg.CallClosures {} -> impossible
          Reg.TailCallClosures {} -> impossible
          Reg.Return x -> goReturn addr x
          Reg.Branch x -> goBranch addr x
          Reg.Case x -> goCase addr x
          Reg.Trace x -> goTrace addr x
          Reg.Dump -> unsupported "dump"
          Reg.Failure x -> goFail addr x
          Reg.Prealloc {} -> return []
          Reg.Nop -> return []
          Reg.Block x -> goBlock addr x

        goConst :: Reg.Constant -> Integer
        goConst = \case
          Reg.ConstInt x -> x
          Reg.ConstBool True -> 0
          Reg.ConstBool False -> 1
          Reg.ConstField f -> fieldToInteger f
          Reg.ConstUnit -> 0
          Reg.ConstVoid -> 0
          Reg.ConstString {} -> unsupported "strings"

        goConstrField :: Reg.ConstrField -> RValue
        goConstrField Reg.ConstrField {..} =
          Load $ LoadValue (goVarRef _constrFieldRef) (toOffset _constrFieldIndex + 1)

        goVarRef :: Reg.VarRef -> MemRef
        goVarRef Reg.VarRef {..} = case _varRefGroup of
          Reg.VarGroupArgs ->
            MemRef Fp (-3 - toOffset _varRefIndex)
          Reg.VarGroupLocal ->
            MemRef Fp (toOffset _varRefIndex)

        goValue :: Reg.Value -> ([Instruction], Value)
        goValue = \case
          Reg.ValConst c -> ([], Imm $ goConst c)
          Reg.CRef x -> ([mkAssignAp (goConstrField x)], Ref $ MemRef Ap (-1))
          Reg.VRef x -> ([], Ref $ goVarRef x)

        goRValue :: Reg.Value -> RValue
        goRValue = \case
          Reg.ValConst c -> Val $ Imm $ goConst c
          Reg.CRef x -> goConstrField x
          Reg.VRef x -> Val $ Ref $ goVarRef x

        goLoad :: Reg.Value -> Offset -> ([Instruction], RValue)
        goLoad v off = case goRValue v of
          Val (Ref r) -> ([], Load $ LoadValue r off)
          v' -> ([mkAssignAp v'], Load $ LoadValue (MemRef Ap (-1)) off)

        goAssignValue :: MemRef -> Reg.Value -> Instruction
        goAssignValue res = mkAssign res . goRValue

        goAssignApValue :: Reg.Value -> Instruction
        goAssignApValue = mkAssignAp . goRValue

        mkBinop :: Reg.BinaryOp -> MemRef -> MemRef -> Value -> [Instruction]
        mkBinop op res arg1 arg2 = case op of
          Reg.OpIntAdd ->
            [mkExtraBinop IntAdd res arg1 arg2]
          Reg.OpIntSub ->
            [mkExtraBinop IntSub res arg1 arg2]
          Reg.OpIntMul ->
            [mkExtraBinop IntMul res arg1 arg2]
          Reg.OpIntDiv ->
            [mkExtraBinop IntDiv res arg1 arg2]
          Reg.OpIntMod ->
            [mkExtraBinop IntMod res arg1 arg2]
          Reg.OpIntLt ->
            [mkExtraBinop IntLt res arg1 arg2]
          Reg.OpIntLe ->
            mkIntLe res arg1 arg2
          Reg.OpFieldAdd ->
            [mkNativeBinop FieldAdd res arg1 arg2]
          Reg.OpFieldSub ->
            [mkExtraBinop FieldSub res arg1 arg2]
          Reg.OpFieldMul ->
            [mkNativeBinop FieldMul res arg1 arg2]
          Reg.OpFieldDiv ->
            [mkExtraBinop FieldDiv res arg1 arg2]
          Reg.OpEq ->
            [mkEq res arg1 arg2]
          Reg.OpStrConcat ->
            unsupported "strings"

        goBinop :: Address -> Reg.InstrBinop -> Sem r [Instruction]
        goBinop addr x@Reg.InstrBinop {..} = case _instrBinopArg1 of
          Reg.ValConst c1 -> case _instrBinopArg2 of
            Reg.ValConst c2 -> case Reg.evalBinop' _instrBinopOpcode c1 c2 of
              Left err -> error err
              Right c ->
                return [mkAssign res (Val $ Imm $ goConst c)]
            _ ->
              goBinop
                addr
                x
                  { Reg._instrBinopArg1 = _instrBinopArg2,
                    Reg._instrBinopArg2 = _instrBinopArg1
                  }
          Reg.CRef ctr1 ->
            case _instrBinopArg2 of
              Reg.CRef {} ->
                return $ i : is2 ++ mkBinop _instrBinopOpcode res (MemRef Ap (-2)) (Ref $ MemRef Ap (-1))
              _ -> do
                eassert (null is2)
                return $ i : mkBinop _instrBinopOpcode res (MemRef Ap (-1)) v2
            where
              i = mkAssignAp (goConstrField ctr1)
          Reg.VRef var1 ->
            return $ is2 ++ mkBinop _instrBinopOpcode res (goVarRef var1) v2
          where
            res = goVarRef _instrBinopResult
            (is2, v2) = goValue _instrBinopArg2

        goUnop :: Address -> Reg.InstrUnop -> Sem r [Instruction]
        goUnop _ Reg.InstrUnop {..} = case _instrUnopOpcode of
          Reg.OpShow -> unsupported "strings"
          Reg.OpStrToInt -> unsupported "strings"
          Reg.OpFieldToInt -> return [goAssignValue res _instrUnopArg]
          Reg.OpIntToField -> return [goAssignValue res _instrUnopArg]
          Reg.OpArgsNum -> case v of
            Ref mr ->
              return $ is ++ mkOpArgsNum res mr
            Imm {} -> impossible
            Lab {} -> impossible
          where
            res = goVarRef _instrUnopResult
            (is, v) = goValue _instrUnopArg

        goAssign :: Address -> Reg.InstrAssign -> Sem r [Instruction]
        goAssign _ Reg.InstrAssign {..} =
          return [goAssignValue res _instrAssignValue]
          where
            res = goVarRef _instrAssignResult

        mkAllocCall :: MemRef -> [Instruction]
        mkAllocCall res =
          [ mkCall $ Lab $ LabelRef (blts ^. stdlibGetRegs) (Just (blts ^. stdlibGetRegsName)),
            mkNativeBinop FieldAdd res (MemRef Ap (-2)) (Imm 2)
          ]

        goAlloc :: Address -> Reg.InstrAlloc -> Sem r [Instruction]
        goAlloc _ Reg.InstrAlloc {..} =
          return $
            mkAllocCall res
              ++ [ mkAssignAp (Val $ Imm $ fromIntegral tagId)
                 ]
              ++ map goAssignApValue _instrAllocArgs
          where
            res = goVarRef _instrAllocResult
            tagId = getTagId _instrAllocTag

        goAllocClosure :: Address -> Reg.InstrAllocClosure -> Sem r [Instruction]
        goAllocClosure _ Reg.InstrAllocClosure {..} =
          return $
            mkAllocCall res
              ++ [ mkAssignAp (Val $ Lab $ LabelRef _instrAllocClosureSymbol (Just funName)),
                   mkAssignAp (Val $ Imm $ fromIntegral $ casmMaxFunctionArgs + 1 - storedArgsNum),
                   mkAssignAp (Val $ Imm $ fromIntegral $ casmMaxFunctionArgs + 1 - leftArgsNum)
                 ]
              ++ map goAssignApValue _instrAllocClosureArgs
          where
            res = goVarRef _instrAllocClosureResult
            funName = Reg.lookupFunInfo tab _instrAllocClosureSymbol ^. Reg.functionName
            storedArgsNum = length _instrAllocClosureArgs
            leftArgsNum = _instrAllocClosureExpectedArgsNum - storedArgsNum

        goExtendClosure :: Address -> Reg.InstrExtendClosure -> Sem r [Instruction]
        goExtendClosure _ Reg.InstrExtendClosure {..} =
          return $
            map goAssignApValue _instrExtendClosureArgs
              ++ [ mkAssignAp (Val $ Imm $ fromIntegral $ length _instrExtendClosureArgs),
                   mkAssignAp (Val $ Ref val),
                   mkCall $ Lab $ LabelRef (blts ^. stdlibExtendClosure) (Just (blts ^. stdlibExtendClosureName)),
                   mkAssign res (Val $ Ref $ MemRef Ap (-1))
                 ]
          where
            res = goVarRef _instrExtendClosureResult
            val = goVarRef _instrExtendClosureValue

        goCall' :: Instruction -> Reg.CallType -> [Reg.Value] -> [Instruction]
        goCall' saveOrRet ct args = case ct of
          Reg.CallFun sym ->
            args'
              ++ [ mkCall $ Lab $ LabelRef sym (Just funName),
                   saveOrRet
                 ]
            where
              funName = Reg.lookupFunInfo tab sym ^. Reg.functionName
          Reg.CallClosure cl ->
            args'
              ++ [ mkAssignAp (Val $ Ref $ goVarRef cl),
                   mkCall $ Lab $ LabelRef (blts ^. stdlibCallClosure) (Just (blts ^. stdlibCallClosureName)),
                   saveOrRet
                 ]
          where
            args' = map goAssignApValue (reverse args)

        goCall :: Address -> Reg.InstrCall -> Sem r [Instruction]
        goCall _ Reg.InstrCall {..} =
          return $
            goCall' (mkAssign res (Val $ Ref $ MemRef Ap (-1))) _instrCallType _instrCallArgs
          where
            res = goVarRef _instrCallResult

        -- There is no way to make "proper" tail calls in Cairo, because
        -- the only way to set the `fp` register is via the `call` instruction.
        -- So we just translate tail calls into `call` followed by `ret`.
        goTailCall :: Address -> Reg.InstrTailCall -> Sem r [Instruction]
        goTailCall _ Reg.InstrTailCall {..} =
          return $
            goCall' Return _instrTailCallType _instrTailCallArgs

        goReturn :: Address -> Reg.InstrReturn -> Sem r [Instruction]
        goReturn _ Reg.InstrReturn {..} =
          return $
            [ goAssignApValue _instrReturnValue,
              Return
            ]

        goBranch :: Address -> Reg.InstrBranch -> Sem r [Instruction]
        goBranch addr Reg.InstrBranch {..} = case v of
          Imm c
            | c == 0 -> goCode addr _instrBranchTrue
            | otherwise -> goCode addr _instrBranchFalse
          Ref r -> do
            symFalse <- freshSymbol
            symEnd <- freshSymbol
            let labFalse = LabelRef symFalse Nothing
                labEnd = LabelRef symEnd Nothing
                addr1 = addr + length is + 1
            codeTrue <- goCode addr1 _instrBranchTrue
            let addr2 = addr1 + length codeTrue + 1
            registerLabelAddress symFalse addr2
            codeFalse <- goCode (addr2 + 1) _instrBranchFalse
            registerLabelAddress symEnd (addr2 + 1 + length codeFalse)
            return $
              is
                ++ [mkJumpIf (Lab labFalse) r]
                ++ codeTrue
                ++ [ mkJump (Val $ Lab labEnd),
                     Label labFalse
                   ]
                ++ codeFalse
                ++ [Label labEnd]
          Lab {} -> impossible
          where
            (is, v) = goValue _instrBranchValue

        goCase :: Address -> Reg.InstrCase -> Sem r [Instruction]
        goCase addr Reg.InstrCase {..} = do
          syms <- replicateM (length tags) freshSymbol
          symEnd <- freshSymbol
          let symMap = HashMap.fromList $ zip tags syms
              labs = map (flip LabelRef Nothing) syms
              labEnd = LabelRef symEnd Nothing
              jmps = map (mkJump . Val . Lab) labs
              -- we need the Nop instructions to ensure that the relative jump
              -- offsets in our CASM interpreter correspond to the relative jump
              -- offsets in the Cairo binary representation
              jmps' = concatMap (\i -> [i, Nop]) jmps
              addr1 = addr + length is + 1 + length jmps'
          (addr2, instrs) <- second (concat . reverse) <$> foldM (goCaseBranch symMap labEnd) (addr1, []) _instrCaseBranches
          (addr3, instrs') <- second reverse <$> foldM (goDefaultLabel symMap) (addr2, []) defaultTags
          instrs'' <- maybe (return []) (goCode addr3) _instrCaseDefault
          let addr4 = addr3 + length instrs''
          registerLabelAddress symEnd addr4
          return $ is ++ mkJumpRel v : jmps' ++ instrs ++ instrs' ++ instrs'' ++ [Label labEnd]
          where
            (is, v) = goLoad _instrCaseValue 0
            tags = Reg.lookupInductiveInfo tab _instrCaseInductive ^. Reg.inductiveConstructors
            ctrTags = HashSet.fromList $ map (^. Reg.caseBranchTag) _instrCaseBranches
            defaultTags = filter (not . flip HashSet.member ctrTags) tags

            goCaseBranch :: HashMap Tag Symbol -> LabelRef -> (Address, [[Instruction]]) -> Reg.CaseBranch -> Sem r (Address, [[Instruction]])
            goCaseBranch symMap labEnd (addr', acc') Reg.CaseBranch {..} = do
              let sym = fromJust $ HashMap.lookup _caseBranchTag symMap
                  lab = LabelRef sym Nothing
              registerLabelAddress sym addr'
              instrs <- goCode (addr' + 1) _caseBranchCode
              let instrs' = Label lab : instrs ++ [mkJump (Val $ Lab labEnd)]
              return (addr' + length instrs', instrs' : acc')

            goDefaultLabel :: HashMap Tag Symbol -> (Address, [Instruction]) -> Reg.Tag -> Sem r (Address, [Instruction])
            goDefaultLabel symMap (addr', acc') tag = do
              let sym = fromJust $ HashMap.lookup tag symMap
                  lab = LabelRef sym Nothing
              registerLabelAddress sym addr'
              return (addr' + 1, Label lab : acc')

        goTrace :: Address -> Reg.InstrTrace -> Sem r [Instruction]
        goTrace _ Reg.InstrTrace {..} =
          return [Trace (InstrTrace (goRValue _instrTraceValue))]

        goFail :: Address -> Reg.InstrFailure -> Sem r [Instruction]
        goFail _ Reg.InstrFailure {..} =
          return
            [ Trace (InstrTrace (goRValue _instrFailureValue)),
              mkJump (Val $ Lab failLab)
            ]

        goBlock :: Address -> Reg.InstrBlock -> Sem r [Instruction]
        goBlock addr Reg.InstrBlock {..} =
          goCode addr _instrBlockCode
