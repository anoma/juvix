module Juvix.Compiler.Core.Extra.IdentDependencyInfo where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Extra.Utils
import Juvix.Compiler.Core.Language
import Juvix.Data.DependencyInfo

type IdentDependencyInfo = DependencyInfo Symbol

createIdentDependencyInfo :: InfoTable -> IdentDependencyInfo
createIdentDependencyInfo tab = createDependencyInfo graph startVertices
  where
    graph :: HashMap Symbol (HashSet Symbol)
    graph =
      fmap
        ( \IdentifierInfo {..} ->
            HashSet.map (\Ident {..} -> _identSymbol) $
              getIdents (fromJust $ HashMap.lookup _identifierSymbol (tab ^. identContext))
        )
        (tab ^. infoIdentifiers)

    startVertices :: HashSet Symbol
    startVertices = HashSet.fromList syms

    syms :: [Symbol]
    syms = map (^. identifierSymbol) (HashMap.elems (tab ^. infoIdentifiers))
