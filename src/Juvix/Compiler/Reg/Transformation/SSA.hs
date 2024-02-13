module Juvix.Compiler.Reg.Transformation.SSA where

import Data.Functor.Identity
import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Reg.Data.IndexMap (IndexMap)
import Juvix.Compiler.Reg.Data.IndexMap qualified as IndexMap
import Juvix.Compiler.Reg.Extra
import Juvix.Compiler.Reg.Transformation.Base

computeFunctionSSA :: Code -> Code
computeFunctionSSA =
  snd
    . runIdentity
    . recurseF
      ForwardRecursorSig
        { _forwardFun = \i acc -> return (go i acc),
          _forwardCombine = combine
        }
      mempty
  where
    go :: Instruction -> IndexMap VarRef -> (IndexMap VarRef, Instruction)
    go instr mp = case getResultVar instr' of
      Just vref -> (mp', setResultVar instr' (mkVarRef VarGroupLocal idx))
        where
          (idx, mp') = IndexMap.assign mp vref
      Nothing -> (mp, instr')
      where
        instr' = overValueRefs (adjustVarRef mp) instr

    combine :: Instruction -> NonEmpty (IndexMap VarRef) -> (IndexMap VarRef, Instruction)
    combine instr mps = case instr of
      Branch InstrBranch {..} -> case mps of
        mp1 :| mp2 : []
          | isNothing _instrBranchVar || idx1 == idx2 ->
              (mp, instr)
          | otherwise ->
              ( mp',
                Branch
                  InstrBranch
                    { _instrBranchTrue = assignInBranch _instrBranchTrue idx' idx1,
                      _instrBranchFalse = assignInBranch _instrBranchFalse idx' idx2,
                      ..
                    }
              )
          where
            var = fromJust _instrBranchVar
            idx1 = IndexMap.lookup mp1 var
            idx2 = IndexMap.lookup mp2 var
            mp = IndexMap.combine mp1 mp2
            (idx', mp') = IndexMap.assign mp var
        _ -> impossible
      Case InstrCase {..} -> case mps of
        mp0 :| mps'
          | isNothing _instrCaseVar || all (== head idxs) (NonEmpty.tail idxs) ->
              (mp, instr)
          | otherwise ->
              ( mp',
                Case
                  InstrCase
                    { _instrCaseBranches = brs',
                      _instrCaseDefault = def',
                      ..
                    }
              )
          where
            var = fromJust _instrCaseVar
            idxs = fmap (flip IndexMap.lookup var) mps
            mp = foldr IndexMap.combine mp0 mps'
            (idx', mp') = IndexMap.assign mp var
            n = length _instrCaseBranches
            brs' = zipWithExact updateBranch _instrCaseBranches (take n (toList idxs))
            def' = fmap (\is -> assignInBranch is idx' (last idxs)) _instrCaseDefault

            updateBranch :: CaseBranch -> Index -> CaseBranch
            updateBranch br idx =
              over caseBranchCode (\is -> assignInBranch is idx' idx) br
      _ -> impossible

    adjustVarRef :: IndexMap VarRef -> VarRef -> VarRef
    adjustVarRef mpv vref@VarRef {..} = case _varRefGroup of
      VarGroupArgs -> vref
      VarGroupLocal -> mkVarRef VarGroupLocal (IndexMap.lookup mpv vref)

    assignInBranch :: Code -> Index -> Index -> Code
    assignInBranch is idx idx' =
      is
        ++ [ Assign
               InstrAssign
                 { _instrAssignResult = mkVarRef VarGroupLocal idx,
                   _instrAssignValue = VRef $ mkVarRef VarGroupLocal idx'
                 }
           ]

computeSSA :: InfoTable -> InfoTable
computeSSA = mapT (const computeFunctionSSA)
