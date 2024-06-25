module Juvix.Compiler.Casm.Transformation.Optimize.Peephole where

import Juvix.Compiler.Casm.Language
import Juvix.Compiler.Casm.Transformation.Base

peephole :: [Instruction] -> [Instruction]
peephole = mapT go
  where
    go :: [Instruction] -> [Instruction]
    go = \case
      Jump InstrJump {..} : lab@(Label LabelRef {..}) : is
        | not _instrJumpIncAp,
          Val (Lab (LabelRef sym _)) <- _instrJumpTarget,
          sym == _labelRefSymbol ->
            lab : is
      is -> is
