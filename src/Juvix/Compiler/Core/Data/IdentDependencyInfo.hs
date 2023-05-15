module Juvix.Compiler.Core.Data.IdentDependencyInfo where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Extra.Utils
import Juvix.Compiler.Core.Language

type IdentDependencyInfo = DependencyInfo Symbol

createIdentDependencyInfo :: InfoTable -> IdentDependencyInfo
createIdentDependencyInfo tab = createDependencyInfo graph startVertices
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
    syms = map (^. identifierSymbol) (HashMap.elems (tab ^. infoIdentifiers))

recursiveIdents :: InfoTable -> HashSet Symbol
recursiveIdents = nodesOnCycles . createIdentDependencyInfo
