module Juvix.Compiler.Backend.Cairo.Extra.Deserialization where

import Data.Bits
import Juvix.Compiler.Backend.Cairo.Data.Result
import Juvix.Compiler.Backend.Cairo.Language

deserialize :: Result -> [Element]
deserialize Result {..} = go [] (map (fromHexText . unpack) _resultData)
  where
    fromHexText :: String -> Natural
    fromHexText s
      | isPrefixOf "0x" s = case readHex (drop 2 s) of
          [(n, "")] -> n
          _ -> error ("error parsing field element: " <> pack s)
      | otherwise = error ("not a hexadecimal number: " <> pack s)

    go :: [Element] -> [Natural] -> [Element]
    go acc = \case
      [] ->
        reverse acc
      e : elems ->
        case instr ^. instrOp1Src of
          Op1SrcImm -> case elems of
            [] -> error "expected an immediate"
            e' : elems' ->
              go (ElementImmediate f : ElementInstruction instr : acc) elems'
              where
                f = fieldFromInteger cairoFieldSize (toInteger e')
          _ ->
            go (ElementInstruction instr : acc) elems
        where
          instr =
            Instruction
              { _instrOffDst = fromBiasedRepr (e .&. 0xFFFF),
                _instrOffOp0 = fromBiasedRepr (shiftR e 16 .&. 0xFFFF),
                _instrOffOp1 = fromBiasedRepr (shiftR e 32 .&. 0xFFFF),
                _instrDstReg = goReg (testBit e 48),
                _instrOp0Reg = goReg (testBit e 49),
                _instrOp1Src = goOp1Src (shiftR e 50 .&. 0x7),
                _instrResLogic = goResLogic (shiftR e 53 .&. 0x3),
                _instrPcUpdate = goPcUpdate (shiftR e 55 .&. 0x7),
                _instrApUpdate = goApUpdate (shiftR e 58 .&. 0x3),
                _instrOpcode = goOpcode (shiftR e 60 .&. 0x7)
              }

    fromBiasedRepr :: Natural -> Offset
    fromBiasedRepr e = fromIntegral (toInteger e - (2 :: Integer) ^ (15 :: Integer))

    goReg :: Bool -> Reg
    goReg = \case
      False -> Ap
      True -> Fp

    goOp1Src :: Natural -> Op1Src
    goOp1Src = \case
      0 -> Op1SrcOp0
      1 -> Op1SrcImm
      2 -> Op1SrcFp
      4 -> Op1SrcAp
      _ -> error "invalide op1_src"

    goResLogic :: Natural -> ResLogic
    goResLogic = \case
      0 -> ResOp1
      1 -> ResAdd
      2 -> ResMul
      _ -> error "invalid res_logic"

    goPcUpdate :: Natural -> PcUpdate
    goPcUpdate = \case
      0 -> PcUpdateRegular
      1 -> PcUpdateJump
      2 -> PcUpdateJumpRel
      4 -> PcUpdateJnz
      _ -> error "invalid pc_update"

    goApUpdate :: Natural -> ApUpdate
    goApUpdate = \case
      0 -> ApUpdateRegular
      1 -> ApUpdateAdd
      2 -> ApUpdateInc
      _ -> error "invalid ap_update"

    goOpcode :: Natural -> Opcode
    goOpcode = \case
      0 -> Nop
      1 -> Call
      2 -> Ret
      4 -> AssertEq
      _ -> error "invalid opcode"
