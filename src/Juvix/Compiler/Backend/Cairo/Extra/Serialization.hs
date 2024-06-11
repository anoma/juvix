module Juvix.Compiler.Backend.Cairo.Extra.Serialization where

import Data.Bits
import Juvix.Compiler.Backend.Cairo.Data.Result
import Juvix.Compiler.Backend.Cairo.Language

serialize :: Int -> [Text] -> [Element] -> Result
serialize outputSize builtins elems =
  Result
    { _resultData =
        initializeBuiltins
          ++ instrs
          ++ finalizeBuiltins
          ++ finalizeJump,
      _resultStart = 0,
      _resultEnd = length initializeBuiltins + elemsSize elems + length finalizeBuiltins,
      _resultMain = 0,
      _resultHints = hints,
      _resultBuiltins = "output" : builtins
    }
  where
    builtinsNum :: Natural
    builtinsNum = fromIntegral (length builtins)

    hints :: [(Int, Text)]
    hints = reverse $ snd $ foldl' goHint (pcShift, []) elems

    instrs :: [Text]
    instrs = map toHexText (serialize' elems)

    pcShift :: Int
    pcShift = length initializeBuiltins

    goHint :: (Int, [(Int, Text)]) -> Element -> (Int, [(Int, Text)])
    goHint (addr, acc) = \case
      ElementHint Hint {..} -> (addr, (addr, _hintCode) : acc)
      elt -> (addr + elemSize elt, acc)

    toHexText :: Natural -> Text
    toHexText n = "0x" <> fromString (showHex n "")

    initializeBuiltins :: [Text]
    initializeBuiltins =
      -- ap += allBuiltinsNum
      [ "0x40480017fff7fff",
        toHexText (builtinsNum + 1)
      ]

    finalizeBuiltins :: [Text]
    finalizeBuiltins =
      -- [[fp] + i] = [ap - outputSize + i]
      -- [output_ptr + i] = [ap - outputSize + i]
      map
        ( \i ->
            toHexText (0x4002800080008000 - outputSize' + i + shift i 32)
        )
        [0 .. outputSize' - 1]
        ++
        -- [ap] = [fp] + outputSize; ap++ -- output_ptr
        [ "0x4826800180008000",
          toHexText outputSize'
        ]
        ++
        -- [ap] = [ap - builtinsNum - 2]; ap++
        replicate
          builtinsNum
          (toHexText (0x48107ffe7fff8000 - shift builtinsNum 32))
      where
        outputSize' = fromIntegral outputSize

    finalizeJump :: [Text]
    finalizeJump =
      -- jmp rel 0
      [ "0x10780017fff7fff",
        "0x0"
      ]

serialize' :: [Element] -> [Natural]
serialize' = mapMaybe goElement
  where
    goElement :: Element -> Maybe Natural
    goElement = \case
      ElementInstruction i -> Just $ goInstr i
      ElementImmediate f -> Just $ fieldToNatural f
      ElementHint {} -> Nothing

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
