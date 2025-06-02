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

mkAssign' :: Maybe Text -> MemRef -> RValue -> Instruction
mkAssign' comment mr rv =
  Assign
    InstrAssign
      { _instrAssignResult = mr,
        _instrAssignValue = rv,
        _instrAssignIncAp = False,
        _instrAssignComment = comment
      }

mkAssign :: MemRef -> RValue -> Instruction
mkAssign = mkAssign' Nothing

mkAssignAp' :: Maybe Text -> RValue -> Instruction
mkAssignAp' comment v =
  Assign
    InstrAssign
      { _instrAssignResult = MemRef Ap 0,
        _instrAssignValue = v,
        _instrAssignIncAp = True,
        _instrAssignComment = comment
      }

mkAssignAp :: RValue -> Instruction
mkAssignAp = mkAssignAp' Nothing

mkCallRel :: Value -> Instruction
mkCallRel tgt = Call (InstrCall tgt True Nothing)

mkCallAbs :: Value -> Instruction
mkCallAbs tgt = Call (InstrCall tgt False Nothing)

mkJumpAbs :: RValue -> Instruction
mkJumpAbs tgt = Jump (InstrJump tgt False False Nothing)

mkJumpIf :: Value -> MemRef -> Instruction
mkJumpIf tgt v = JumpIf (InstrJumpIf tgt v False Nothing)

mkJumpRel :: RValue -> Instruction
mkJumpRel tgt = Jump (InstrJump tgt True False Nothing)
