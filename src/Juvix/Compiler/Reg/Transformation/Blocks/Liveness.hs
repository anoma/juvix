module Juvix.Compiler.Reg.Transformation.Blocks.Liveness where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Reg.Extra.Blocks
import Juvix.Compiler.Reg.Transformation.Blocks.Base

computeBlockLiveness :: Block -> Block
computeBlockLiveness block = block' {_blockLiveVars = vars'}
  where
    block' = overBlocks computeBlockLiveness block
    vars0 = HashSet.unions $ map (^. blockLiveVars) (getBlocks block)
    vars1 = updateByFinalInstr (block ^. blockFinal) vars0
    vars' = foldr updateByInstr vars1 (block ^. blockBody)

    updateByFinalInstr :: Maybe FinalInstruction -> HashSet VarRef -> HashSet VarRef
    updateByFinalInstr mi acc = case mi of
      Nothing -> acc
      Just i -> updateBy (getResultVar' i) (getValueRefs' i) acc

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
