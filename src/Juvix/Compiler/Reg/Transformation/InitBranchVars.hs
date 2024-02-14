module Juvix.Compiler.Reg.Transformation.InitBranchVars where

import Data.Functor.Identity
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Juvix.Compiler.Reg.Extra
import Juvix.Compiler.Reg.Transformation.Base

initBranchVars :: InfoTable -> InfoTable
initBranchVars = mapT (const goFun)
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

    go :: Code -> HashSet VarRef -> [HashSet VarRef] -> (HashSet VarRef, Code)
    go is a as = case is of
      Branch InstrBranch {..} : is' -> case as of
        [a1, a2] -> (a <> a', i' : is')
          where
            a' = a1 <> a2
            a1' = HashSet.difference a' a1
            a2' = HashSet.difference a' a2
            i' =
              Branch
                InstrBranch
                  { _instrBranchTrue = addInits a1' _instrBranchTrue,
                    _instrBranchFalse = addInits a2' _instrBranchFalse,
                    ..
                  }
        _ -> impossible
      Case InstrCase {..} : is' ->
        (a <> a', i' : is')
        where
          a' = mconcat as
          as' = map (HashSet.difference a') as
          n = length _instrCaseBranches
          brs' = zipWithExact goBranch (take n as') _instrCaseBranches
          def' = maybe Nothing (Just . addInits (List.last as')) _instrCaseDefault
          i' =
            Case
              InstrCase
                { _instrCaseBranches = brs',
                  _instrCaseDefault = def',
                  ..
                }

          goBranch :: HashSet VarRef -> CaseBranch -> CaseBranch
          goBranch vars = over caseBranchCode (addInits vars)
      i : _ ->
        case getResultVar i of
          Just v ->
            (HashSet.insert v a <> mconcat as, is)
          Nothing ->
            (a <> mconcat as, is)
      [] ->
        (a <> mconcat as, is)

    addInits :: HashSet VarRef -> Code -> Code
    addInits vars is = is ++ map mk (toList vars)
      where
        mk :: VarRef -> Instruction
        mk vref =
          Assign
            InstrAssign
              { _instrAssignResult = vref,
                _instrAssignValue = Const ConstVoid
              }
