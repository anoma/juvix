module Juvix.Compiler.Core.Transformation.UnrollRecursion (unrollRecursion) where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Data.IdentDependencyInfo
import Juvix.Compiler.Core.Data.InfoTableBuilder
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base

unrollRecursion :: InfoTable -> InfoTable
unrollRecursion tab =
  let (mp, (tab', ())) =
        run $
          runState @(HashMap Symbol Symbol) mempty $
            runInfoTableBuilder tab $
              forM_ (buildSCCs (createIdentDependencyInfo tab)) goSCC
   in mapIdentSymbols mp $ pruneInfoTable tab'
  where
    mapIdentSymbols :: HashMap Symbol Symbol -> InfoTable -> InfoTable
    mapIdentSymbols mp = over infoMain adjustMain . mapAllNodes (umap go)
      where
        go :: Node -> Node
        go = \case
          NIdt idt@Ident {..} ->
            case HashMap.lookup _identSymbol mp of
              Just sym' -> NIdt idt {_identSymbol = sym'}
              Nothing -> NIdt idt
          node -> node

        adjustMain :: Maybe Symbol -> Maybe Symbol
        adjustMain = \case
          Just sym -> Just $ fromMaybe sym (HashMap.lookup sym mp)
          Nothing -> Nothing

    goSCC :: Members '[InfoTableBuilder, State (HashMap Symbol Symbol)] r => SCC Symbol -> Sem r ()
    goSCC = \case
      CyclicSCC syms -> unrollSCC syms
      AcyclicSCC _ -> return ()

    unrollSCC :: Members '[InfoTableBuilder, State (HashMap Symbol Symbol)] r => [Symbol] -> Sem r ()
    unrollSCC syms = do
      freshSyms <- genSyms
      forM_ syms (unroll freshSyms)
      modify (\mp -> foldr (mapSymbol freshSyms) mp syms)
      where
        unrollLimit :: Int
        unrollLimit = 140

        mapSymbol :: HashMap (Symbol, Int) Symbol -> Symbol -> HashMap Symbol Symbol -> HashMap Symbol Symbol
        mapSymbol freshSyms sym = HashMap.insert sym (fromJust $ HashMap.lookup (sym, unrollLimit) freshSyms)

        genSyms :: forall r. Member InfoTableBuilder r => Sem r (HashMap (Symbol, Int) Symbol)
        genSyms = foldr go (return mempty) syms
          where
            go :: Symbol -> Sem r (HashMap (Symbol, Int) Symbol) -> Sem r (HashMap (Symbol, Int) Symbol)
            go sym m = foldr (go' sym) m [0 .. unrollLimit]

            go' :: Symbol -> Int -> Sem r (HashMap (Symbol, Int) Symbol) -> Sem r (HashMap (Symbol, Int) Symbol)
            go' sym limit m = do
              mp <- m
              sym' <- freshSymbol
              return $ HashMap.insert (sym, limit) sym' mp

        unroll :: forall r. Member InfoTableBuilder r => HashMap (Symbol, Int) Symbol -> Symbol -> Sem r ()
        unroll freshSyms sym = do
          forM_ [0 .. unrollLimit] goUnroll
          removeSymbol sym
          where
            ii = fromJust $ HashMap.lookup sym (tab ^. infoIdentifiers)

            goUnroll :: Int -> Sem r ()
            goUnroll limit = do
              let sym' = fromJust $ HashMap.lookup (sym, limit) freshSyms
                  name' = ii ^. identifierName <> "__" <> show limit
                  ii' = ii {_identifierSymbol = sym', _identifierName = name'}
              registerIdent name' ii'
              let node =
                    if
                        | limit == 0 ->
                            etaExpand (typeArgs (ii ^. identifierType)) (mkBuiltinApp' OpFail [mkConstant' (ConstString "recursion limit reached")])
                        | otherwise ->
                            umap (go limit) (fromJust $ HashMap.lookup sym (tab ^. identContext))
              registerIdentNode sym' node

            go :: Int -> Node -> Node
            go limit = \case
              NIdt idt@Ident {..} ->
                NIdt idt {_identSymbol = fromMaybe _identSymbol $ HashMap.lookup (_identSymbol, limit - 1) freshSyms}
              node -> node
