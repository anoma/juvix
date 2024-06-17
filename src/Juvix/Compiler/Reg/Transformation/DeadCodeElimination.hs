module Juvix.Compiler.Reg.Transformation.DeadCodeElimination where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Reg.Extra
import Juvix.Compiler.Reg.Transformation.Base

removeDeadAssignments :: InfoTable -> InfoTable
removeDeadAssignments = mapT (const goFun)
  where
    goFun :: Code -> Code
    goFun =
      snd
        . runIdentity
        . recurseB
          BackwardRecursorSig
            { _backwardFun = \is a as -> return (go is a as),
              _backwardAdjust = const mempty
            }
          mempty

    -- The accumulator contains live variables
    go :: Code -> HashSet VarRef -> [HashSet VarRef] -> (HashSet VarRef, Code)
    go is live lives = case is of
      instr : is' -> undefined
        where
          liveVars = case instr of
            Branch {} -> ulives
            Case {} -> ulives
            _ -> live
          ulives = HashSet.unions lives
      [] -> impossible
