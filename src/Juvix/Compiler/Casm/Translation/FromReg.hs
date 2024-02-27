module Juvix.Compiler.Casm.Translation.FromReg where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Backend
import Juvix.Compiler.Casm.Data.LabelInfo
import Juvix.Compiler.Casm.Data.LabelInfoBuilder
import Juvix.Compiler.Casm.Extra.Base
import Juvix.Compiler.Casm.Extra.Stdlib
import Juvix.Compiler.Casm.Language
import Juvix.Compiler.Reg.Data.InfoTable qualified as Reg
import Juvix.Compiler.Reg.Extra.Info qualified as Reg
import Juvix.Compiler.Reg.Language qualified as Reg
import Juvix.Compiler.Tree.Evaluator.Builtins qualified as Reg
import Juvix.Data.Field

fromReg :: Reg.InfoTable -> (LabelInfo, [Instruction])
fromReg tab = run $ runLabelInfoBuilderWithNextId (Reg.getNextSymbolId tab) $ do
  let initialOffset :: Int = 2
  (blts, binstrs) <- addStdlibBuiltins
  (addr, instrs) <- second (concat . reverse) <$> foldM (goFun blts) (initialOffset + length binstrs, []) (tab ^. Reg.infoFunctions)
  eassert (addr == length instrs + length binstrs + initialOffset)
  let endName :: Text = "__juvix_end"
  endSym <- freshSymbol
  registerLabelName endSym endName
  registerLabelAddress endSym addr
  let mainSym = fromJust $ tab ^. Reg.infoMainFunction
      mainName = fromJust (HashMap.lookup mainSym (tab ^. Reg.infoFunctions)) ^. Reg.functionName
      callInstr = Call $ InstrCall $ Lab $ LabelRef mainSym (Just mainName)
      jmpInstr =
        Jump $
          InstrJump
            { _instrJumpTarget = Lab $ LabelRef endSym (Just endName),
              _instrJumpIncAp = False
            }
  return $ callInstr : jmpInstr : binstrs ++ instrs
  where
    info :: Reg.ExtraInfo
    info = Reg.computeExtraInfo (getLimits TargetCairo False) tab

    goFun :: forall r. (Member LabelInfoBuilder r) => StdlibBuiltins -> (Address, [[Instruction]]) -> Reg.FunctionInfo -> Sem r (Address, [[Instruction]])
    goFun blts (addr0, acc) funInfo = do
      sym <- freshSymbol
      registerLabelName sym (funInfo ^. Reg.functionName)
      registerLabelAddress sym addr0
      let code = funInfo ^. Reg.functionCode
          n = fromJust $ HashMap.lookup (funInfo ^. Reg.functionSymbol) (info ^. Reg.extraInfoLocalVarsNum)
          i1 = Alloc $ InstrAlloc $ Val $ Imm $ fromIntegral n
          pre = [i1]
          addr1 = addr0 + length pre
      instrs <- goCode addr1 code
      return (addr1 + length instrs, (pre ++ instrs) : acc)
      where
        unsupported :: Text -> a
        unsupported what = error ("Cairo backend: unsupported: " <> what)

        goCode :: Address -> Reg.Code -> Sem r [Instruction]
        goCode addr = concatMapM (goInstr addr)

        goInstr :: Address -> Reg.Instruction -> Sem r [Instruction]
        goInstr addr = \case
          Reg.Binop x -> goBinop addr x
          Reg.Unop x -> goUnop addr x
          Reg.Assign x -> goAssign addr x
          Reg.Alloc x -> goAlloc addr x
          Reg.AllocClosure x -> goAllocClosure addr x
          Reg.ExtendClosure x -> goExtendClosure addr x
          Reg.Call x -> goCall addr x
          Reg.CallClosures x -> goCallClosures addr x
          Reg.TailCall x -> goTailCall addr x
          Reg.TailCallClosures x -> goTailCallClosures addr x
          Reg.Return x -> goReturn addr x
          Reg.Branch x -> goBranch addr x
          Reg.Case x -> goCase addr x
          Reg.Trace x -> goTrace addr x
          Reg.Dump -> unsupported "dump"
          Reg.Failure {} -> unsupported "fail"
          Reg.Prealloc {} -> return []
          Reg.Nop -> return []
          Reg.Block x -> goBlock addr x

        goConst :: Reg.Constant -> Integer
        goConst = \case
          Reg.ConstInt x -> x
          Reg.ConstBool True -> 1
          Reg.ConstBool False -> 0
          Reg.ConstField f -> fieldToInteger f
          Reg.ConstUnit -> 0
          Reg.ConstVoid -> 0
          Reg.ConstString {} -> unsupported "strings"

        goConstrField :: MemRef -> Bool -> Reg.ConstrField -> Instruction
        goConstrField mref incAp Reg.ConstrField {..} =
          Assign
            InstrAssign
              { _instrAssignValue = Load $ LoadValue (goVarRef _constrFieldRef) (toOffset _constrFieldIndex),
                _instrAssignResult = mref,
                _instrAssignIncAp = incAp
              }

        goVarRef :: Reg.VarRef -> MemRef
        goVarRef Reg.VarRef {..} = case _varRefGroup of
          Reg.VarGroupArgs ->
            MemRef Fp (-3 - toOffset _varRefIndex)
          Reg.VarGroupLocal ->
            MemRef Fp (toOffset _varRefIndex)

        goValue :: Reg.Value -> ([Instruction], Value)
        goValue = \case
          Reg.Const c -> ([], Imm $ goConst c)
          Reg.CRef x -> ([goConstrField (MemRef Ap 0) True x], Ref $ MemRef Ap (-1))
          Reg.VRef x -> ([], Ref $ goVarRef x)

        goAssignValue :: MemRef -> Reg.Value -> Instruction
        goAssignValue res = \case
          Reg.Const c -> mkAssign res (Val $ Imm $ goConst c)
          Reg.CRef x -> goConstrField res False x
          Reg.VRef x -> mkAssign res (Val $ Ref $ goVarRef x)

        goAssignApValue :: Reg.Value -> Instruction
        goAssignApValue = \case
          Reg.Const c -> mkAssignAp (Val $ Imm $ goConst c)
          Reg.CRef x -> goConstrField (MemRef Ap 0) True x
          Reg.VRef x -> mkAssignAp (Val $ Ref $ goVarRef x)

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
          Reg.Const c1 -> case _instrBinopArg2 of
            Reg.Const c2 -> case Reg.evalBinop' _instrBinopOpcode c1 c2 of
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
              Reg.CRef {} -> do
                let i = goConstrField (MemRef Ap 0) True ctr1
                return $ i : is2 ++ mkBinop _instrBinopOpcode res (MemRef Ap (-2)) (Ref $ MemRef Ap (-1))
              _ -> do
                eassert (null is2)
                let i = goConstrField (MemRef Ap 0) True ctr1
                return $ i : mkBinop _instrBinopOpcode res (MemRef Ap (-1)) v2
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

        goAlloc :: Address -> Reg.InstrAlloc -> Sem r [Instruction]
        goAlloc _ Reg.InstrAlloc {..} =
          return $
            [ mkCall $ Lab $ LabelRef (blts ^. stdlibGetRegs) (Just (blts ^. stdlibGetRegsName)),
              mkNativeBinop FieldAdd res (MemRef Ap (-2)) (Imm 2),
              mkAssignAp (Val $ Imm $ fromIntegral tagId)
            ]
              ++ map goAssignApValue _instrAllocArgs
          where
            res = goVarRef _instrAllocResult
            tagId = fromJust $ HashMap.lookup _instrAllocTag (info ^. Reg.extraInfoUIDs)

        goAllocClosure :: Address -> Reg.InstrAllocClosure -> Sem r [Instruction]
        goAllocClosure addr Reg.InstrAllocClosure {..} = undefined

        goExtendClosure :: Address -> Reg.InstrExtendClosure -> Sem r [Instruction]
        goExtendClosure addr Reg.InstrExtendClosure {..} = undefined

        goCall :: Address -> Reg.InstrCall -> Sem r [Instruction]
        goCall addr Reg.InstrCall {..} = undefined

        goTailCall :: Address -> Reg.InstrTailCall -> Sem r [Instruction]
        goTailCall addr Reg.InstrTailCall {..} = undefined

        goCallClosures :: Address -> Reg.InstrCallClosures -> Sem r [Instruction]
        goCallClosures addr Reg.InstrCallClosures {..} = undefined

        goTailCallClosures :: Address -> Reg.InstrTailCallClosures -> Sem r [Instruction]
        goTailCallClosures addr Reg.InstrTailCallClosures {..} = undefined

        goReturn :: Address -> Reg.InstrReturn -> Sem r [Instruction]
        goReturn addr Reg.InstrReturn {..} = undefined

        goBranch :: Address -> Reg.InstrBranch -> Sem r [Instruction]
        goBranch addr Reg.InstrBranch {..} = undefined

        goCase :: Address -> Reg.InstrCase -> Sem r [Instruction]
        goCase addr Reg.InstrCase {..} = undefined

        goTrace :: Address -> Reg.InstrTrace -> Sem r [Instruction]
        goTrace addr Reg.InstrTrace {..} = undefined

        goBlock :: Address -> Reg.InstrBlock -> Sem r [Instruction]
        goBlock addr Reg.InstrBlock {..} = undefined
