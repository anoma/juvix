module Juvix.Compiler.Casm.Extra.Base where

import Juvix.Compiler.Casm.Language

toOffset :: (Show a, Integral a) => a -> Offset
toOffset x
  | fromIntegral v == x = v
  | otherwise = error ("invalid offset: " <> show x)
  where
    v = fromIntegral x

adjustAp :: Int16 -> MemRef -> MemRef
adjustAp idx mr@MemRef {..} = case _memRefReg of
  Ap -> MemRef Ap (_memRefOff - idx)
  Fp -> mr

mkAssign :: MemRef -> RValue -> Instruction
mkAssign mr rv =
  Assign
    InstrAssign
      { _instrAssignResult = mr,
        _instrAssignValue = rv,
        _instrAssignIncAp = False
      }

mkAssignAp :: RValue -> Instruction
mkAssignAp v =
  Assign
    InstrAssign
      { _instrAssignResult = MemRef Ap 0,
        _instrAssignValue = v,
        _instrAssignIncAp = True
      }

mkCallRel :: Value -> Instruction
mkCallRel tgt = Call (InstrCall tgt True)

mkCallAbs :: Value -> Instruction
mkCallAbs tgt = Call (InstrCall tgt False)

mkJumpAbs :: RValue -> Instruction
mkJumpAbs tgt = Jump (InstrJump tgt False False)

mkJumpIf :: Value -> MemRef -> Instruction
mkJumpIf tgt v = JumpIf (InstrJumpIf tgt v False)

mkJumpRel :: RValue -> Instruction
mkJumpRel tgt = Jump (InstrJump tgt True False)
