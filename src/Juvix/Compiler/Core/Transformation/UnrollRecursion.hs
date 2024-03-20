module Juvix.Compiler.Core.Transformation.UnrollRecursion (unrollRecursion) where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Data.IdentDependencyInfo
import Juvix.Compiler.Core.Data.InfoTableBuilder
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.TypeInfo (setNodeType)
import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Transformation.Base

unrollRecursion :: (Member (Reader CoreOptions) r) => Module -> Sem r Module
unrollRecursion md = do
  (mp, md') <-
    runState @(HashMap Symbol Symbol) mempty
      . execInfoTableBuilder md
      . forM_ (buildSCCs (createCallGraph (md ^. moduleInfoTable)))
      $ goSCC
  return . mapIdentSymbols mp $ pruneInfoTable md'
  where
    mapIdentSymbols :: HashMap Symbol Symbol -> Module -> Module
    mapIdentSymbols mp = over (moduleInfoTable . infoMain) adjustMain . mapAllNodes (umap go)
      where
        go :: Node -> Node
        go = \case
          NIdt idt@Ident {..} ->
            case HashMap.lookup _identSymbol mp of
              Just sym' -> NIdt idt {_identSymbol = sym'}
              Nothing -> NIdt idt
          node -> node

        adjustMain :: Maybe Symbol -> Maybe Symbol
        adjustMain = fmap $ \sym -> fromMaybe sym (HashMap.lookup sym mp)

    goSCC :: (Members '[InfoTableBuilder, State (HashMap Symbol Symbol), Reader CoreOptions] r) => SCC Symbol -> Sem r ()
    goSCC = \case
      CyclicSCC syms -> unrollSCC syms
      AcyclicSCC _ -> return ()

    unrollSCC :: (Members '[InfoTableBuilder, State (HashMap Symbol Symbol), Reader CoreOptions] r) => [Symbol] -> Sem r ()
    unrollSCC syms = do
      unrollLimit <- computeUnrollLimit
      freshSyms <- genSyms unrollLimit syms
      forM_ syms (unroll unrollLimit freshSyms)
      modify (\mp -> foldr (mapSymbol unrollLimit freshSyms) mp syms)
      where
        computeUnrollLimit :: (Member (Reader CoreOptions) r) => Sem r Int
        computeUnrollLimit = do
          defaultUnrollLimit <- asks (^. optUnrollLimit)
          let lims = nonEmpty (mapMaybe go syms)
          return (maybe defaultUnrollLimit minimum1 lims)
          where
            go :: Symbol -> Maybe Int
            go sym = fmap (^. pragmaUnrollDepth) (ii ^. identifierPragmas . pragmasUnroll)
              where
                ii = lookupIdentifierInfo md sym

        mapSymbol :: Int -> HashMap (Indexed Symbol) Symbol -> Symbol -> HashMap Symbol Symbol -> HashMap Symbol Symbol
        mapSymbol unrollLimit freshSyms sym = HashMap.insert sym (fromJust $ HashMap.lookup (Indexed unrollLimit sym) freshSyms)

        genSyms :: forall r. (Member InfoTableBuilder r) => Int -> [Symbol] -> Sem r (HashMap (Indexed Symbol) Symbol)
        genSyms unrollLimit = foldr go (return mempty)
          where
            go :: Symbol -> Sem r (HashMap (Indexed Symbol) Symbol) -> Sem r (HashMap (Indexed Symbol) Symbol)
            go sym m = foldr (go' sym) m [0 .. unrollLimit]

            go' :: Symbol -> Int -> Sem r (HashMap (Indexed Symbol) Symbol) -> Sem r (HashMap (Indexed Symbol) Symbol)
            go' sym limit m = do
              mp <- m
              sym' <- freshSymbol
              return $ HashMap.insert (Indexed limit sym) sym' mp

        unroll :: forall r. (Member InfoTableBuilder r) => Int -> HashMap (Indexed Symbol) Symbol -> Symbol -> Sem r ()
        unroll unrollLimit freshSyms sym = do
          forM_ [0 .. unrollLimit] goUnroll
          removeSymbol sym
          where
            ii = lookupIdentifierInfo md sym

            goUnroll :: Int -> Sem r ()
            goUnroll limit = do
              let sym' = fromJust $ HashMap.lookup (Indexed limit sym) freshSyms
                  name' = ii ^. identifierName <> "__" <> show limit
                  ii' = ii {_identifierSymbol = sym', _identifierName = name'}
              registerIdent name' ii'
              let failNode =
                    setNodeType (ii ^. identifierType) $
                      mkBuiltinApp' OpFail [mkConstant' (ConstString "recursion limit reached")]
                  node
                    | limit == 0 =
                        etaExpand (typeArgs (ii ^. identifierType)) failNode
                    | otherwise =
                        umap (go limit) (lookupIdentifierNode md sym)
              registerIdentNode sym' node

            go :: Int -> Node -> Node
            go limit = \case
              NIdt idt@Ident {..} ->
                NIdt idt {_identSymbol = fromMaybe _identSymbol $ HashMap.lookup (Indexed (limit - 1) _identSymbol) freshSyms}
              node -> node
