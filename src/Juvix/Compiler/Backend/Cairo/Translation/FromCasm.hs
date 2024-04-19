module Juvix.Compiler.Backend.Cairo.Translation.FromCasm where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Backend.Cairo.Language
import Juvix.Compiler.Casm.Data.LabelInfo qualified as Casm
import Juvix.Compiler.Casm.Extra.LabelInfo qualified as Casm
import Juvix.Compiler.Casm.Language qualified as Casm

fromCasm :: [Casm.Instruction] -> [Element]
fromCasm instrs0 =
  reverse $ snd $ foldl' (goInstr' (Casm.computeLabelInfo' instrSize instrs0)) (0, []) instrs0
  where
    unsupported :: Text -> a
    unsupported msg = error ("Cairo: unsupported: " <> msg)

    labelSyms =
      mapMaybe
        ( \case
            Casm.Label (Casm.LabelRef sym _) -> Just sym
            _ -> Nothing
        )
        instrs0

    instrSize :: Casm.Instruction -> Int
    instrSize = elemsSize . goInstr dummyLabInfo 0
      where
        -- This LabelInfo has incorrect offsets -- it is used only to implement
        -- `instrSize` needed to compute the correct LabelInfo
        dummyLabInfo = Casm.LabelInfo $ HashMap.fromList $ zip labelSyms (repeat 0)

    goInstr' :: Casm.LabelInfo -> (Address, [Element]) -> Casm.Instruction -> (Address, [Element])
    goInstr' labInfo (addr, acc) i = (addr + elemsSize is, reverse is ++ acc)
      where
        is = goInstr labInfo addr i

    goInstr :: Casm.LabelInfo -> Address -> Casm.Instruction -> [Element]
    goInstr labInfo addr = \case
      Casm.Assign x -> goAssign x
      Casm.ExtraBinop x -> goExtraBinop x
      Casm.Jump x -> goJump x
      Casm.JumpIf x -> goJumpIf x
      Casm.Call x -> goCall x
      Casm.Return -> goReturn
      Casm.Alloc x -> goAlloc x
      Casm.Hint x -> goHint x
      Casm.Trace {} -> []
      Casm.Label {} -> []
      Casm.Nop -> []
      where
        updateDst :: Casm.MemRef -> Instruction -> Instruction
        updateDst Casm.MemRef {..} instr =
          instr
            { _instrOffDst = _memRefOff,
              _instrDstReg = _memRefReg
            }

        updateOp0 :: Casm.MemRef -> Instruction -> Instruction
        updateOp0 Casm.MemRef {..} instr =
          instr
            { _instrOffOp0 = _memRefOff,
              _instrOp0Reg = _memRefReg
            }

        updateOp1 :: Bool -> Casm.Value -> Instruction -> (Instruction, Maybe FField)
        updateOp1 isRel v instr = case v of
          Casm.Imm x ->
            (instr', Just (fieldFromInteger cairoFieldSize x))
            where
              instr' =
                instr
                  { _instrOp1Src = Op1SrcImm,
                    _instrOffOp1 = 1
                  }
          Casm.Ref Casm.MemRef {..} ->
            (instr', Nothing)
            where
              src = case _memRefReg of
                Ap -> Op1SrcAp
                Fp -> Op1SrcFp
              instr' =
                instr
                  { _instrOp1Src = src,
                    _instrOffOp1 = _memRefOff
                  }
          Casm.Lab (Casm.LabelRef sym _) ->
            (instr', Just (fieldFromInteger cairoFieldSize (fromIntegral labAddr)))
            where
              labValue = fromJust $ HashMap.lookup sym (labInfo ^. Casm.labelInfoTable)

              labAddr :: Address
              labAddr
                | isRel = labValue - addr
                | otherwise = labValue

              instr' =
                instr
                  { _instrOp1Src = Op1SrcImm,
                    _instrOffOp1 = 1
                  }

        updateOps :: Bool -> Casm.RValue -> Instruction -> (Instruction, Maybe FField)
        updateOps isRel rv instr = case rv of
          Casm.Val v ->
            updateOp1 isRel v instr
          Casm.Load Casm.LoadValue {..} ->
            (instr'', Nothing)
            where
              instr' = updateOp0 _loadValueSrc instr
              instr'' =
                instr'
                  { _instrOp1Src = Op1SrcOp0,
                    _instrOffOp1 = _loadValueOff
                  }
          Casm.Binop Casm.BinopValue {..} ->
            (instr'', mf)
            where
              (instr', mf) = updateOp1 isRel _binopValueArg2 (updateOp0 _binopValueArg1 instr)
              instr'' =
                instr'
                  { _instrResLogic = case _binopValueOpcode of
                      Casm.FieldAdd -> ResAdd
                      Casm.FieldMul -> ResMul
                  }

        updateIncAp :: Bool -> Instruction -> Instruction
        updateIncAp incAp instr =
          instr
            { _instrApUpdate = if incAp then ApUpdateInc else ApUpdateRegular
            }

        toElems :: (Instruction, Maybe FField) -> [Element]
        toElems (instr, mf) =
          ElementInstruction instr : maybeToList (fmap ElementImmediate mf)

        goAssign :: Casm.InstrAssign -> [Element]
        goAssign Casm.InstrAssign {..} =
          toElems
            . updateOps False _instrAssignValue
            . updateDst _instrAssignResult
            . updateIncAp _instrAssignIncAp
            . set instrOpcode AssertEq
            $ defaultInstruction

        goBinop :: Bool -> Casm.Opcode -> Casm.MemRef -> Casm.MemRef -> Casm.Value -> [Element]
        goBinop incAp op res arg1 arg2 =
          toElems
            . updateOps False (Casm.Binop $ Casm.BinopValue op arg1 arg2)
            . updateDst res
            . updateIncAp incAp
            . set instrOpcode AssertEq
            $ defaultInstruction

        goExtraBinop :: Casm.InstrExtraBinop -> [Element]
        goExtraBinop Casm.InstrExtraBinop {..} = case _instrExtraBinopOpcode of
          Casm.FieldSub ->
            goBinop _instrExtraBinopIncAp Casm.FieldAdd _instrExtraBinopArg1 _instrExtraBinopResult _instrExtraBinopArg2
          Casm.FieldDiv ->
            goBinop _instrExtraBinopIncAp Casm.FieldMul _instrExtraBinopArg1 _instrExtraBinopResult _instrExtraBinopArg2
          Casm.IntAdd ->
            -- TODO: range check
            goBinop _instrExtraBinopIncAp Casm.FieldAdd _instrExtraBinopResult _instrExtraBinopArg1 _instrExtraBinopArg2
          Casm.IntSub ->
            -- TODO: range check
            goBinop _instrExtraBinopIncAp Casm.FieldAdd _instrExtraBinopArg1 _instrExtraBinopResult _instrExtraBinopArg2
          Casm.IntMul ->
            -- TODO: range check
            goBinop _instrExtraBinopIncAp Casm.FieldMul _instrExtraBinopResult _instrExtraBinopArg1 _instrExtraBinopArg2
          Casm.IntDiv ->
            unsupported "integer division"
          Casm.IntMod ->
            unsupported "integer modulus"
          Casm.IntLt ->
            unsupported "integer comparison"

        goJump :: Casm.InstrJump -> [Element]
        goJump Casm.InstrJump {..} =
          toElems
            . updateOps _instrJumpRel _instrJumpTarget
            . updateIncAp _instrJumpIncAp
            . set instrPcUpdate pcUpdate
            $ defaultInstruction
          where
            pcUpdate = if _instrJumpRel then PcUpdateJumpRel else PcUpdateJump

        goJumpIf :: Casm.InstrJumpIf -> [Element]
        goJumpIf Casm.InstrJumpIf {..} =
          toElems
            . updateOp1 True _instrJumpIfTarget
            . updateDst _instrJumpIfValue
            . updateIncAp _instrJumpIfIncAp
            . set instrPcUpdate PcUpdateJnz
            $ defaultInstruction

        goCall :: Casm.InstrCall -> [Element]
        goCall Casm.InstrCall {..} =
          toElems
            . updateOp1 _instrCallRel _instrCallTarget
            $ defaultInstruction
              { _instrOffDst = 0,
                _instrDstReg = Ap,
                _instrOffOp0 = 1,
                _instrOp0Reg = Ap,
                _instrPcUpdate = pcUpdate,
                _instrOpcode = Call
              }
          where
            pcUpdate = if _instrCallRel then PcUpdateJumpRel else PcUpdateJump

        goReturn :: [Element]
        goReturn =
          [ ElementInstruction $
              Instruction
                { _instrOffDst = -2,
                  _instrOffOp0 = -1,
                  _instrOffOp1 = -1,
                  _instrDstReg = Fp,
                  _instrOp0Reg = Fp,
                  _instrOp1Src = Op1SrcFp,
                  _instrResLogic = ResOp1,
                  _instrPcUpdate = PcUpdateJump,
                  _instrApUpdate = ApUpdateRegular,
                  _instrOpcode = Ret
                }
          ]

        goAlloc :: Casm.InstrAlloc -> [Element]
        goAlloc Casm.InstrAlloc {..} =
          toElems
            . updateOps False _instrAllocSize
            . set instrApUpdate ApUpdateAdd
            $ defaultInstruction

        goHint :: Casm.Hint -> [Element]
        goHint = \case
          Casm.HintInput var -> [ElementHint (Hint ("Input(" <> var <> ")"))]
          Casm.HintAlloc size -> [ElementHint (Hint ("Alloc(" <> show size <> ")"))]
