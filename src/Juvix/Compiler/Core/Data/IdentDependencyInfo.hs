module Juvix.Compiler.Core.Data.IdentDependencyInfo where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Extra.Utils
import Juvix.Compiler.Core.Language

-- | Call graph type
type IdentDependencyInfo = DependencyInfo Symbol

-- | Compute the call graph
createCallGraph :: InfoTable -> IdentDependencyInfo
createCallGraph tab = createDependencyInfo graph startVertices
  where
    graph :: HashMap Symbol (HashSet Symbol)
    graph =
      fmap
        ( \IdentifierInfo {..} ->
            HashSet.map (\Ident {..} -> _identSymbol) $
              getIdents (lookupIdentifierNode tab _identifierSymbol)
        )
        (tab ^. infoIdentifiers)

    startVertices :: HashSet Symbol
    startVertices = HashSet.fromList syms

    syms :: [Symbol]
    syms = maybe [] singleton (tab ^. infoMain)

createSymbolDependencyInfo :: InfoTable -> IdentDependencyInfo
createSymbolDependencyInfo tab = createDependencyInfo graph startVertices
  where
    graph :: HashMap Symbol (HashSet Symbol)
    graph =
      fmap
        ( \IdentifierInfo {..} ->
            getSymbols tab (lookupIdentifierNode tab _identifierSymbol)
        )
        (tab ^. infoIdentifiers)
        <> foldr
          ( \ConstructorInfo {..} ->
              HashMap.insert _constructorInductive (getSymbols tab _constructorType)
          )
          mempty
          (tab ^. infoConstructors)

    startVertices :: HashSet Symbol
    startVertices = HashSet.fromList syms

    syms :: [Symbol]
    syms = maybe [] singleton (tab ^. infoMain)

recursiveIdents :: InfoTable -> HashSet Symbol
recursiveIdents = nodesOnCycles . createCallGraph
