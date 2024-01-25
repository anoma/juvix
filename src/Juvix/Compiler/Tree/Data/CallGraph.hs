module Juvix.Compiler.Tree.Data.CallGraph where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Tree.Data.InfoTable
import Juvix.Compiler.Tree.Extra.Recursors

-- | Call graph type
type CallGraph = DependencyInfo Symbol

-- | Compute the call graph
createCallGraph :: InfoTable -> CallGraph
createCallGraph tab = createDependencyInfo (createCallGraphMap tab) startVertices
  where
    startVertices :: HashSet Symbol
    startVertices = HashSet.fromList syms

    syms :: [Symbol]
    syms = maybe [] singleton (tab ^. infoMainFunction)

createCallGraphMap :: InfoTable -> HashMap Symbol (HashSet Symbol)
createCallGraphMap tab = fmap (getFunSymbols . (^. functionCode)) (tab ^. infoFunctions)

getFunSymbols :: Node -> HashSet Symbol
getFunSymbols = gather go mempty
  where
    go :: HashSet Symbol -> Node -> HashSet Symbol
    go syms = \case
      AllocClosure NodeAllocClosure {..} -> HashSet.insert _nodeAllocClosureFunSymbol syms
      Call NodeCall {..} -> goCallType syms _nodeCallType
      _ -> syms

    goCallType :: HashSet Symbol -> CallType -> HashSet Symbol
    goCallType syms = \case
      CallFun sym -> HashSet.insert sym syms
      CallClosure {} -> syms
