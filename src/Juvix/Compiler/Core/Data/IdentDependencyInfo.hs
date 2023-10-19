module Juvix.Compiler.Core.Data.IdentDependencyInfo where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Extra.Utils
import Juvix.Compiler.Core.Language

-- | Call graph type
type IdentDependencyInfo = DependencyInfo Symbol

createCallGraphMap :: InfoTable -> HashMap Symbol (HashSet Symbol)
createCallGraphMap tab =
  fmap
    ( \IdentifierInfo {..} ->
        HashSet.map (\Ident {..} -> _identSymbol) $
          getIdents (lookupIdentifierNode tab _identifierSymbol)
    )
    (tab ^. infoIdentifiers)

-- | Compute the call graph
createCallGraph :: InfoTable -> IdentDependencyInfo
createCallGraph tab = createDependencyInfo graph startVertices
  where
    graph :: HashMap Symbol (HashSet Symbol)
    graph = createCallGraphMap tab

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

-- | identifiers from which some recursive identifier can be reached
recursiveIdentsClosure :: InfoTable -> HashSet Symbol
recursiveIdentsClosure tab =
  -- unfortunately, there is no Graph library function which would allow to
  -- compute this in linear time; hence, we implement this directly
  run .
    evalState (mempty :: HashSet Symbol) $
      foldM (dfs mempty) mempty (HashMap.keys graph)
  where
    graph = createCallGraphMap tab

    dfs :: (Member (State (HashSet Symbol)) r) => HashSet Symbol -> HashSet Symbol -> Symbol -> Sem r (HashSet Symbol)
    dfs path acc sym = do
      visited <- get
      if
          | HashSet.member sym visited ->
              return acc
          | otherwise -> do
              let path' = HashSet.insert sym path
                  acc' =
                    if
                        | any (`HashSet.member` path') chlds ->
                            HashSet.insert sym acc
                        | otherwise ->
                            acc
              modify' (HashSet.insert sym)
              acc'' <- foldM (dfs path') acc' chlds
              if
                  | any (`HashSet.member` acc'') chlds ->
                      return $ HashSet.insert sym acc''
                  | otherwise ->
                      return acc''
      where
        chlds = fromJust $ HashMap.lookup sym graph

-- | Complement of recursiveIdentsClosure
nonRecursiveIdents :: InfoTable -> HashSet Symbol
nonRecursiveIdents tab =
  HashSet.difference
    (HashSet.fromList (HashMap.keys (tab ^. infoIdentifiers)))
    (recursiveIdentsClosure tab)
