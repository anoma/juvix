module Juvix.Compiler.Backend.Cairo.Extra.Serialization where

import Data.Bits
import Juvix.Compiler.Backend.Cairo.Data.Result
import Juvix.Compiler.Backend.Cairo.Language
import Numeric

serialize :: [Element] -> Result
serialize elems =
  Result
    { _resultData = map toHexText (serialize' elems)
    }
  where
    toHexText :: Natural -> Text
    toHexText n = "0x" <> fromString (showHex n "")

serialize' :: [Element] -> [Natural]
serialize' = map goElement
  where
    goElement :: Element -> Natural
    goElement = \case
      ElementInstruction i -> goInstr i
      ElementImmediate f -> fieldToNatural f

    goInstr :: Instruction -> Natural
    goInstr Instruction {..} =
      toBiasedRepr _instrOffDst
        .|. shift (toBiasedRepr _instrOffOp0) 16
        .|. shift (toBiasedRepr _instrOffOp1) 32
        .|. shift (goReg _instrDstReg) 48
        .|. shift (goReg _instrOp0Reg) 49
        .|. shift (goOp1Src _instrOp1Src) 50
        .|. shift (goResLogic _instrResLogic) 53
        .|. shift (goPcUpdate _instrPcUpdate) 55
        .|. shift (goApUpdate _instrApUpdate) 58
        .|. shift (goOpcode _instrOpcode) 60

    toBiasedRepr :: Offset -> Natural
    toBiasedRepr off = fromIntegral (fromIntegral @Int16 @Integer off + (2 :: Integer) ^ (15 :: Integer))

    goReg :: Reg -> Natural
    goReg = \case
      Ap -> 0
      Fp -> 1

    goOp1Src :: Op1Src -> Natural
    goOp1Src = \case
      Op1SrcOp0 -> 0
      Op1SrcImm -> 1
      Op1SrcFp -> 2
      Op1SrcAp -> 4

    goResLogic :: ResLogic -> Natural
    goResLogic = \case
      ResOp1 -> 0
      ResAdd -> 1
      ResMul -> 2

    goPcUpdate :: PcUpdate -> Natural
    goPcUpdate = \case
      PcUpdateRegular -> 0
      PcUpdateJump -> 1
      PcUpdateJumpRel -> 2
      PcUpdateJnz -> 4

    goApUpdate :: ApUpdate -> Natural
    goApUpdate = \case
      ApUpdateRegular -> 0
      ApUpdateAdd -> 1
      ApUpdateInc -> 2

    goOpcode :: Opcode -> Natural
    goOpcode = \case
      Nop -> 0
      Call -> 1
      Ret -> 2
      AssertEq -> 4
