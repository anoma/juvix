module Juvix.Compiler.Reg.Data.Blocks.CallGraph where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Reg.Data.Blocks.InfoTable
import Juvix.Compiler.Reg.Language.Blocks

type CallGraph = DependencyInfo Symbol

createCallGraph :: InfoTable -> CallGraph
createCallGraph tab =
  createDependencyInfo (createCallGraphMap tab) startVertices
  where
    startVertices :: HashSet Symbol
    startVertices = HashSet.fromList syms

    syms :: [Symbol]
    syms = maybe [] singleton (tab ^. infoMainFunction)

createCallGraphMap :: InfoTable -> HashMap Symbol (HashSet Symbol)
createCallGraphMap tab =
  fmap
    (\FunctionInfo {..} -> getFunSymbols _functionCode)
    (tab ^. infoFunctions)

getFunSymbols :: Block -> HashSet Symbol
getFunSymbols block =
  HashSet.union
    (maybe mempty goFinalInstr (block ^. blockFinal))
    (foldr goInstr (maybe mempty getFunSymbols (block ^. blockNext)) (block ^. blockBody))
  where
    goInstr :: Instruction -> HashSet Symbol -> HashSet Symbol
    goInstr = \case
      AllocClosure InstrAllocClosure {..} -> HashSet.insert _instrAllocClosureSymbol
      _ -> id

    goFinalInstr :: FinalInstruction -> HashSet Symbol
    goFinalInstr = \case
      ExtendClosure {} -> mempty
      Call InstrCall {..}
        | CallFun sym <- _instrCallType -> HashSet.fromList [sym]
        | otherwise -> mempty
      TailCall InstrTailCall {..}
        | CallFun sym <- _instrTailCallType -> HashSet.fromList [sym]
        | otherwise -> mempty
      Return {} -> mempty
      Branch InstrBranch {..} ->
        HashSet.union (getFunSymbols _instrBranchTrue) (getFunSymbols _instrBranchFalse)
      Case InstrCase {..} ->
        foldr (HashSet.union . goCaseBranch) (maybe mempty getFunSymbols _instrCaseDefault) _instrCaseBranches

    goCaseBranch :: CaseBranch -> HashSet Symbol
    goCaseBranch = getFunSymbols . (^. caseBranchCode)
