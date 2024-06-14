module Juvix.Compiler.Reg.Transformation.CopyPropagation where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Reg.Extra
import Juvix.Compiler.Reg.Transformation.Base

type VarMap = HashMap VarRef VarRef

copyPropagateFunction :: Code -> Code
copyPropagateFunction =
  snd
    . runIdentity
    . recurseF
      ForwardRecursorSig
        { _forwardFun = \i acc -> return (go i acc),
          _forwardCombine = combine
        }
      mempty
  where
    go :: Instruction -> VarMap -> (VarMap, Instruction)
    go instr mpv = case instr' of
      Assign InstrAssign {..}
        | VRef v <- _instrAssignValue ->
            (HashMap.insert _instrAssignResult v mpv', instr')
      _ ->
        (mpv', instr')
      where
        instr' = overValueRefs (adjustVarRef mpv) instr
        mpv' = maybe mpv (filterOutVars mpv) (getResultVar instr')

    filterOutVars :: VarMap -> VarRef -> VarMap
    filterOutVars mpv v = HashMap.delete v $ HashMap.filter (/= v) mpv

    adjustVarRef :: VarMap -> VarRef -> VarRef
    adjustVarRef mpv vref@VarRef {..} = case _varRefGroup of
      VarGroupArgs -> vref
      VarGroupLocal -> fromMaybe vref $ HashMap.lookup vref mpv

    combine :: Instruction -> NonEmpty VarMap -> (VarMap, Instruction)
    combine instr mpvs = (mpv, instr')
      where
        mpv' :| mpvs' = fmap HashMap.toList mpvs
        mpv =
          HashMap.fromList
            . HashSet.toList
            . foldr (HashSet.intersection . HashSet.fromList) (HashSet.fromList mpv')
            $ mpvs'

        instr' = case instr of
          Branch x -> Branch $ over instrBranchOutVar (fmap (adjustVarRef mpv)) x
          Case x -> Case $ over instrCaseOutVar (fmap (adjustVarRef mpv)) x
          _ -> impossible

copyPropagate :: InfoTable -> InfoTable
copyPropagate = mapT (const copyPropagateFunction)
