module Juvix.Compiler.Reg.Transformation.SSA where

import Data.Functor.Identity
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
        mp1 :| mp2 : [] -> undefined
        _ -> impossible
      Case InstrCase {..} -> undefined
      _ -> impossible

    adjustVarRef :: IndexMap VarRef -> VarRef -> VarRef
    adjustVarRef mpv vref@VarRef {..} = case _varRefGroup of
      VarGroupArgs -> vref
      VarGroupLocal -> mkVarRef VarGroupLocal (IndexMap.lookup mpv vref)

computeSSA :: InfoTable -> InfoTable
computeSSA = mapT (const computeFunctionSSA)
