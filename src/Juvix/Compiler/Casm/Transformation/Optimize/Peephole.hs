module Juvix.Compiler.Casm.Transformation.Optimize.Peephole where

import Juvix.Compiler.Casm.Extra.Base
import Juvix.Compiler.Casm.Language
import Juvix.Compiler.Casm.Transformation.Base

peephole :: [Instruction] -> [Instruction]
peephole = mapT go
  where
    go :: [Instruction] -> [Instruction]
    go = \case
      Nop : is -> is
      Jump InstrJump {..} : lab@(Label LabelRef {..}) : is
        | not _instrJumpIncAp,
          Val (Lab (LabelRef sym _)) <- _instrJumpTarget,
          sym == _labelRefSymbol ->
            lab : is
      Call InstrCall {..} : Return : Assign a1 : Return : is
        | _instrCallRel,
          Imm 3 <- _instrCallTarget,
          Just k1 <- getAssignApFp a1 ->
            fixAssignAp $
              mkAssignAp (Val (Ref (MemRef Ap k1)))
                : Return
                : is
      Call InstrCall {..} : Return : Assign a1 : Assign a2 : Return : is
        | _instrCallRel,
          Imm 3 <- _instrCallTarget,
          Just k1 <- getAssignApFp a1,
          Just k2 <- getAssignApFp a2 ->
            fixAssignAp $
              mkAssignAp (Val (Ref (MemRef Ap k1)))
                : mkAssignAp (Val (Ref (MemRef Ap (k2 - 1))))
                : Return
                : is
      Call InstrCall {..} : Return : Jump InstrJump {..} : is
        | _instrCallRel,
          Imm 3 <- _instrCallTarget,
          Val tgt@(Lab {}) <- _instrJumpTarget,
          not _instrJumpIncAp ->
            let call =
                  InstrCall
                    { _instrCallTarget = tgt,
                      _instrCallRel = _instrJumpRel
                    }
             in Call call : Return : is
      is -> is

    fixAssignAp :: [Instruction] -> [Instruction]
    fixAssignAp = \case
      Assign a : Return : is
        | Just (-1) <- getAssignAp Ap a ->
            Return : is
      Assign a1 : Assign a2 : Return : is
        | Just (-2) <- getAssignAp Ap a1,
          Just (-2) <- getAssignAp Ap a2 ->
            Return : is
      Assign a1 : Assign a2 : Return : is
        | Just (-1) <- getAssignAp Ap a1,
          Just (-3) <- getAssignAp Ap a2 ->
            mkAssignAp (Val (Ref (MemRef Ap (-2)))) : Return : is
      is -> is

    getAssignAp :: Reg -> InstrAssign -> Maybe Offset
    getAssignAp reg InstrAssign {..}
      | MemRef Ap 0 <- _instrAssignResult,
        Val (Ref (MemRef r k)) <- _instrAssignValue,
        r == reg,
        _instrAssignIncAp =
          Just k
      | otherwise =
          Nothing

    getAssignApFp :: InstrAssign -> Maybe Offset
    getAssignApFp instr = case getAssignAp Fp instr of
      Just k
        | k <= -3 -> Just (k + 2)
      _ -> Nothing
