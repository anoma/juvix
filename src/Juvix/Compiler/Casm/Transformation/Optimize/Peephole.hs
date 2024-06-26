module Juvix.Compiler.Casm.Transformation.Optimize.Peephole where

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
      Call InstrCall {..} : Return : Return : is
        | _instrCallRel,
          Imm 3 <- _instrCallTarget ->
            Return : is
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
