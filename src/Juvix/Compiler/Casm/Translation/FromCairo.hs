module Juvix.Compiler.Casm.Translation.FromCairo where

import Juvix.Compiler.Backend.Cairo.Language qualified as Cairo
import Juvix.Compiler.Casm.Data.Result
import Juvix.Compiler.Casm.Language

fromCairo :: [Cairo.Element] -> Result
fromCairo elems0 = Result mempty (go 0 [] elems0) mempty
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
    goInstr addr instr1 elems1 = case instr1 ^. Cairo.instrOpcode of
      Cairo.Call -> goCall instr1 elems1
      Cairo.Ret -> goRet instr1 elems1
      Cairo.AssertEq -> goAssertEq instr1 elems1
      Cairo.Nop -> goNop instr1 elems1
      where
        getImm :: Cairo.Element -> Immediate
        getImm = \case
          Cairo.ElementImmediate imm -> Cairo.fieldToInteger imm
          _ -> errorMsg (addr + 1) "expected an immediate value"

        getOp1Val :: RValue -> Value
        getOp1Val = \case
          Val val -> val
          _ -> errorMsg addr ("unexpected op1 load: " <> show instr1)

        decodeDst :: Cairo.Instruction -> MemRef
        decodeDst Cairo.Instruction {..} = MemRef _instrDstReg _instrOffDst

        decodeRes :: Cairo.Instruction -> [Cairo.Element] -> (RValue, Int)
        decodeRes Cairo.Instruction {..} elems = (res, delta)
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

        goCall :: Cairo.Instruction -> [Cairo.Element] -> (Instruction, Int)
        goCall i@Cairo.Instruction {..} elems
          | (_instrPcUpdate == Cairo.PcUpdateJump || _instrPcUpdate == Cairo.PcUpdateJumpRel)
              && _instrApUpdate == Cairo.ApUpdateRegular
              && _instrDstReg == Ap
              && _instrOffDst == 0 =
              (call, delta)
          | otherwise =
              errorMsg addr ("invalid call: " <> show i)
          where
            call =
              Call
                InstrCall
                  { _instrCallRel = _instrPcUpdate == Cairo.PcUpdateJumpRel,
                    _instrCallTarget = val
                  }
            (rval, delta) = decodeRes i elems
            val = case rval of
              Val v -> v
              _ -> errorMsg addr ("invalid call: " <> show i)

        goRet :: Cairo.Instruction -> [Cairo.Element] -> (Instruction, Int)
        goRet i@Cairo.Instruction {..} _
          | _instrApUpdate == Cairo.ApUpdateRegular
              && _instrPcUpdate == Cairo.PcUpdateJump
              && _instrResLogic == Cairo.ResOp1
              && _instrOp1Src == Cairo.Op1SrcFp
              && _instrOffOp1 == -1
              && _instrDstReg == Fp
              && _instrOffDst == -2 =
              (Return, 0)
          | otherwise =
              errorMsg addr ("invalid ret: " <> show i)

        goAssertEq :: Cairo.Instruction -> [Cairo.Element] -> (Instruction, Int)
        goAssertEq i@Cairo.Instruction {..} elems
          | (_instrApUpdate == Cairo.ApUpdateInc || _instrApUpdate == Cairo.ApUpdateRegular)
              && _instrPcUpdate == Cairo.PcUpdateRegular =
              (asng, delta)
          | otherwise =
              errorMsg addr ("invalid assignment: " <> show i)
          where
            (rval, delta) = decodeRes i elems
            dst = decodeDst i
            asng =
              Assign
                InstrAssign
                  { _instrAssignResult = dst,
                    _instrAssignValue = rval,
                    _instrAssignIncAp = _instrApUpdate == Cairo.ApUpdateInc
                  }

        goNop :: Cairo.Instruction -> [Cairo.Element] -> (Instruction, Int)
        goNop i@Cairo.Instruction {..} elems =
          case _instrPcUpdate of
            Cairo.PcUpdateJnz -> goJumpIf i elems
            Cairo.PcUpdateJump -> goJump False i elems
            Cairo.PcUpdateJumpRel -> goJump True i elems
            Cairo.PcUpdateRegular -> case _instrApUpdate of
              Cairo.ApUpdateAdd -> goAlloc i elems
              _ -> errorMsg addr ("cannot disassemble: " <> show i)

        goJump :: Bool -> Cairo.Instruction -> [Cairo.Element] -> (Instruction, Int)
        goJump isRel i@Cairo.Instruction {..} elems
          | (_instrApUpdate == Cairo.ApUpdateInc || _instrApUpdate == Cairo.ApUpdateRegular) =
              (jmp, delta)
          | otherwise =
              errorMsg addr ("invalid jump: " <> show i)
          where
            (res, delta) = decodeRes i elems
            jmp =
              Jump
                InstrJump
                  { _instrJumpTarget = res,
                    _instrJumpRel = isRel,
                    _instrJumpIncAp = _instrApUpdate == Cairo.ApUpdateInc
                  }

        goJumpIf :: Cairo.Instruction -> [Cairo.Element] -> (Instruction, Int)
        goJumpIf i@Cairo.Instruction {..} elems
          | (_instrApUpdate == Cairo.ApUpdateInc || _instrApUpdate == Cairo.ApUpdateRegular) =
              case res of
                Val val ->
                  (jmp val, delta)
                _ ->
                  errorMsg addr ("cannot disassemble conditional jump: " <> show i)
          | otherwise =
              errorMsg addr ("invalid conditional jump: " <> show i)
          where
            (res, delta) = decodeRes i elems
            dst = decodeDst i

            jmp :: Value -> Instruction
            jmp tgt =
              JumpIf
                InstrJumpIf
                  { _instrJumpIfTarget = tgt,
                    _instrJumpIfValue = dst,
                    _instrJumpIfIncAp = _instrApUpdate == Cairo.ApUpdateInc
                  }

        goAlloc :: Cairo.Instruction -> [Cairo.Element] -> (Instruction, Int)
        goAlloc i elems = (alloc, delta)
          where
            (res, delta) = decodeRes i elems
            alloc =
              Alloc
                InstrAlloc
                  { _instrAllocSize = res
                  }
