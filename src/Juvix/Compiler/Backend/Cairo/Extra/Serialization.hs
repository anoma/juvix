module Juvix.Compiler.Backend.Cairo.Extra.Serialization where

import Data.Bits
import Juvix.Compiler.Backend.Cairo.Data.Result
import Juvix.Compiler.Backend.Cairo.Language
import Numeric

serialize :: [Element] -> Result
serialize elems =
  Result
    { _resultData =
        initializeOutput
          ++ map toHexText (serialize' elems)
          ++ finalizeOutput
          ++ finalizeJump,
      _resultStart = 0,
      _resultEnd = length initializeOutput + length elems + length finalizeOutput,
      _resultMain = 0,
      _resultHints = hints,
      _resultBuiltins = ["output"]
    }
  where
    hints :: [(Int, Text)]
    hints = catMaybes $ zipWith mkHint elems [0 ..]

    mkHint :: Element -> Int -> Maybe (Int, Text)
    mkHint el pc = case el of
      ElementHint Hint {..} -> Just (pc, _hintCode)
      _ -> Nothing

    toHexText :: Natural -> Text
    toHexText n = "0x" <> fromString (showHex n "")

    initializeOutput :: [Text]
    initializeOutput =
      [ "0x40480017fff7fff",
        "0x1"
      ]

    finalizeOutput :: [Text]
    finalizeOutput =
      [ "0x4002800080007fff",
        "0x4826800180008000",
        "0x1"
      ]

    finalizeJump :: [Text]
    finalizeJump =
      [ "0x10780017fff7fff",
        "0x0"
      ]

serialize' :: [Element] -> [Natural]
serialize' = map goElement
  where
    goElement :: Element -> Natural
    goElement = \case
      ElementInstruction i -> goInstr i
      ElementImmediate f -> fieldToNatural f
      ElementHint h -> goHint h

    goHint :: Hint -> Natural
    goHint Hint {..}
      | _hintIncAp = 0x481280007fff8000
      | otherwise = 0x401280007fff8000

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
