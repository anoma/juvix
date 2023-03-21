module Juvix.Compiler.Core.Data.TypeDependencyInfo where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Extra.Utils
import Juvix.Compiler.Core.Language

type TypeDependencyInfo = DependencyInfo Symbol

createTypeDependencyInfo :: InfoTable -> TypeDependencyInfo
createTypeDependencyInfo tab = createDependencyInfo graph startVertices
  where
    graph :: HashMap Symbol (HashSet Symbol)
    graph =
      fmap
        ( \InductiveInfo {..} ->
            foldr
              (mappend . getInductives)
              mempty
              ( concatMap
                  (\ci -> typeArgs (ci ^. constructorType))
                  _inductiveConstructors
              )
        )
        (HashMap.filter (isNothing . (^. inductiveBuiltin)) (tab ^. infoInductives))

    startVertices :: HashSet Symbol
    startVertices = HashSet.fromList syms

    syms :: [Symbol]
    syms = map (^. inductiveSymbol) (HashMap.elems (tab ^. infoInductives))
