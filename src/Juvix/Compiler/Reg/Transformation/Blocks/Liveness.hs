module Juvix.Compiler.Reg.Transformation.Blocks.Liveness where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Reg.Extra.Blocks
import Juvix.Compiler.Reg.Transformation.Blocks.Base

-- | The live variables in the `next` block are not considered live in the
-- case/branch subblocks. For example, in:
--    br x { y := a } { y := b }; z := c
-- the variable `c` is not considered live inside the branches. Only `a` is
-- live in the first branch, and only `b` in the second. Before `br` the live
-- variables are: x, a, b, c.
-- The liveness notion here is suited for Cairo compilation, where it indicates
-- whether a variable should be transferred over a basic block boundary. The
-- basic block ending with the `br` instruction can transfer to the branches (`y
-- := a` and `y := b`) and to the `next` block consisting of `z := c`, but the
-- blocks corresponding to the branches do not directly transfer to the `next`
-- block.
computeBlockLiveness :: Block -> Block
computeBlockLiveness block = block' {_blockLiveVars = vars'}
  where
    block' = overBlocks computeBlockLiveness block
    vars0 = HashSet.unions $ map (^. blockLiveVars) (getBlocks block')
    vars1 = updateByFinalInstr (block ^. blockFinal) vars0
    vars' = foldr updateByInstr vars1 (block ^. blockBody)

    updateByFinalInstr :: Maybe FinalInstruction -> HashSet VarRef -> HashSet VarRef
    updateByFinalInstr mi acc = case mi of
      Nothing -> acc
      Just i -> updateBy (getOutVar i) (getValueRefs' i) acc

    updateByInstr :: Instruction -> HashSet VarRef -> HashSet VarRef
    updateByInstr i acc = updateBy (getResultVar i) (getValueRefs i) acc

    updateBy :: Maybe VarRef -> [VarRef] -> HashSet VarRef -> HashSet VarRef
    updateBy mr vs acc = acc2
      where
        acc1 = case mr of
          Nothing -> acc
          Just x -> HashSet.delete x acc
        acc2 = HashSet.union acc1 (HashSet.fromList vs)

computeLiveness :: InfoTable -> InfoTable
computeLiveness = mapT (const computeBlockLiveness)
