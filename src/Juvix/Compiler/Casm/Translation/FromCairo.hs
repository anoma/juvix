module Juvix.Compiler.Casm.Translation.FromCairo where

import Juvix.Compiler.Backend.Cairo.Language qualified as Cairo
import Juvix.Compiler.Casm.Data.Result
import Juvix.Compiler.Casm.Language

fromCairo :: [Cairo.Element] -> Result
fromCairo elems0 = Result mempty (go 0 [] elems0)
  where
    errorMsg :: Address -> Text -> a
    errorMsg addr msg = error ("error at address " <> show addr <> ": " <> msg)

    go :: Address -> [Instruction] -> [Cairo.Element] -> [Instruction]
    go addr acc = \case
      [] -> reverse acc
      Cairo.ElementInstruction instr : elems ->
        go (addr + delta + 1) (i : acc) (drop delta elems)
        where
          (i, delta) = goInstr addr instr elems
      Cairo.ElementImmediate {} : _ -> errorMsg addr "cannot disassemble an immediate value"
      Cairo.ElementHint {} : _ -> errorMsg addr "cannot disassemble a hint"

    goInstr :: Address -> Cairo.Instruction -> [Cairo.Element] -> (Instruction, Int)
    goInstr addr i@Cairo.Instruction {..} elems = case _instrOpcode of
      Cairo.Call
        | (_instrPcUpdate == Cairo.PcUpdateJump || _instrPcUpdate == Cairo.PcUpdateJumpRel)
            && _instrApUpdate == Cairo.ApUpdateRegular
            && _instrDstReg == Ap
            && _instrOffDst == 0 ->
            (call, delta)
        | otherwise ->
            errorMsg addr ("invalid call: " <> show i)
        where
          call =
            Call
              InstrCall
                { _instrCallRel = _instrPcUpdate == Cairo.PcUpdateJumpRel,
                  _instrCallTarget = val
                }
          (rval, delta) = decodeRes
          val = case rval of
            Val v -> v
            _ -> errorMsg addr ("invalid call: " <> show i)
      Cairo.Ret
        | _instrApUpdate == Cairo.ApUpdateRegular
            && _instrPcUpdate == Cairo.PcUpdateJump
            && _instrResLogic == Cairo.ResOp1
            && _instrOp1Src == Cairo.Op1SrcFp
            && _instrOffOp1 == -1
            && _instrDstReg == Fp
            && _instrOffDst == -2 ->
            (Return, 0)
        | otherwise ->
            errorMsg addr ("invalid ret: " <> show i)
      Cairo.AssertEq
        | (_instrApUpdate == Cairo.ApUpdateInc || _instrApUpdate == Cairo.ApUpdateRegular)
            && _instrPcUpdate == Cairo.PcUpdateRegular ->
            (asng, delta)
        | otherwise ->
            errorMsg addr ("invalid assignment: " <> show i)
        where
          (rval, delta) = decodeRes
          dst = decodeDst
          asng =
            Assign
              InstrAssign
                { _instrAssignResult = dst,
                  _instrAssignValue = rval,
                  _instrAssignIncAp = _instrApUpdate == Cairo.ApUpdateInc
                }
      Cairo.Nop -> case _instrPcUpdate of
        Cairo.PcUpdateJnz -> undefined
        Cairo.PcUpdateJump -> decodeJump False
        Cairo.PcUpdateJumpRel -> decodeJump True
        Cairo.PcUpdateRegular -> case _instrApUpdate of
          Cairo.ApUpdateAdd -> undefined
          _ -> errorMsg addr ("cannot disassemble: " <> show i)
      where
        getImm :: Cairo.Element -> Immediate
        getImm = \case
          Cairo.ElementImmediate imm -> Cairo.fieldToInteger imm
          _ -> errorMsg (addr + 1) "expected an immediate value"

        getOp1Val :: RValue -> Value
        getOp1Val = \case
          Val val -> val
          _ -> errorMsg addr ("unexpected op1 load: " <> show i)

        decodeDst :: MemRef
        decodeDst = MemRef _instrDstReg _instrOffDst

        decodeRes :: (RValue, Int)
        decodeRes = (res, delta)
          where
            delta = if _instrOp1Src == Cairo.Op1SrcImm then 1 else 0
            op0 = MemRef _instrOp0Reg _instrOffOp0
            op1 = case _instrOp1Src of
              Cairo.Op1SrcOp0 -> Load $ LoadValue op0 _instrOffOp1
              Cairo.Op1SrcAp -> Val $ Ref $ MemRef Ap _instrOffOp1
              Cairo.Op1SrcFp -> Val $ Ref $ MemRef Fp _instrOffOp1
              Cairo.Op1SrcImm -> Val $ Imm $ getImm (head' elems)
            res = case _instrResLogic of
              Cairo.ResOp1 -> op1
              Cairo.ResAdd -> Binop $ BinopValue FieldAdd op0 (getOp1Val op1)
              Cairo.ResMul -> Binop $ BinopValue FieldMul op0 (getOp1Val op1)

        decodeJump :: Bool -> (Instruction, Int)
        decodeJump isRel
          | (_instrApUpdate == Cairo.ApUpdateInc || _instrApUpdate == Cairo.ApUpdateRegular) =
              (jmp, delta)
          | otherwise =
              errorMsg addr ("invalid jump: " <> show i)
          where
            (res, delta) = decodeRes
            jmp =
              Jump
                InstrJump
                  { _instrJumpTarget = res,
                    _instrJumpRel = isRel,
                    _instrJumpIncAp = _instrApUpdate == Cairo.ApUpdateInc
                  }
