module Juvix.Compiler.Core.Transformation.Optimize.CaseCallLifting (caseCallLifting) where

import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base

convertNode :: InfoTable -> Node -> Node
convertNode tab = umap go
  where
    go :: Node -> Node
    go = \case
      NCase Case {..}
        | not (null idents) ->
            if
                | isCaseBoolean _caseBranches && not (isImmediate tab _caseValue) ->
                    mkLet'
                      mkTypeBool'
                      _caseValue
                      (liftApps 0 _caseInductive (mkVar' 0) (brs' 1) (def' 1) idents)
                | otherwise ->
                    liftApps 0 _caseInductive _caseValue (brs' 0) (def' 0) idents
        where
          bodies = map (^. caseBranchBody) _caseBranches ++ maybeToList _caseDefault
          idts = foldr (flip gatherIdents) mempty bodies
          idents = filter (\sym -> all (\x -> countApps sym x == 1) bodies) (toList idts)
          n = length idents
          brs' k = map (over caseBranchBody (shift (n + k))) _caseBranches
          def' k = fmap (shift (n + k)) _caseDefault
      node -> node

    liftApps :: Level -> Symbol -> Node -> [CaseBranch] -> Maybe Node -> [Symbol] -> Node
    liftApps lvl ind val brs def = \case
      [] ->
        NCase
          Case
            { _caseInfo = mempty,
              _caseInductive = ind,
              _caseValue = shift lvl val,
              _caseBranches = brs,
              _caseDefault = def
            }
      sym : syms -> mkLet' ty app (liftApps (lvl + 1) ind val brs' def' syms)
        where
          idx = length syms
          args0 = map (fromJust . gatherAppArgs sym . (^. caseBranchBody)) brs
          dargs0 = fmap (fromJust . gatherAppArgs sym) def
          appArgs = computeArgs args0 dargs0
          app = mkApps' (mkIdent' sym) appArgs
          (tyargs, tgt) = unfoldPi' (lookupIdentifierInfo tab sym ^. identifierType)
          tyargs' = drop (length appArgs) tyargs
          ty = substs appArgs (mkPis' tyargs' tgt)
          brs' = map (\br -> over caseBranchBody (substApps sym (mkVar' (br ^. caseBranchBindersNum + idx))) br) brs
          def' = fmap (substApps sym (mkVar' idx)) def

          computeArgs :: [[Node]] -> Maybe [Node] -> [Node]
          computeArgs args dargs
            | null (List.head args) = []
            | otherwise =
                shift
                  (-idx - 1)
                  (mkCase' ind (shift (lvl + 1) val) (zipWithExact (set caseBranchBody) hbs brs) hdef)
                  : computeArgs args' dargs'
            where
              hbs = map List.head args
              hdef = fmap List.head dargs
              args' = map List.tail args
              dargs' = fmap List.tail dargs

    gatherIdents :: HashSet Symbol -> Node -> HashSet Symbol
    gatherIdents = sgather go'
      where
        go' :: HashSet Symbol -> Node -> HashSet Symbol
        go' acc node = case node of
          NApp {} ->
            let (h, args) = unfoldApps' node
             in case h of
                  NIdt Ident {..}
                    | length args == lookupIdentifierInfo tab _identSymbol ^. identifierArgsNum ->
                        HashSet.insert _identSymbol acc
                  _ -> acc
          _ -> acc

    countApps :: Symbol -> Node -> Int
    countApps sym = sgather go' 0
      where
        argsNum = lookupIdentifierInfo tab sym ^. identifierArgsNum

        go' :: Int -> Node -> Int
        go' acc node = case node of
          NApp {} ->
            let (h, args) = unfoldApps' node
             in case h of
                  NIdt Ident {..}
                    | _identSymbol == sym
                        && length args == argsNum ->
                        acc + 1
                  _ -> acc
          _ -> acc

    gatherAppArgs :: Symbol -> Node -> Maybe [Node]
    gatherAppArgs sym = sgather go' Nothing
      where
        argsNum = lookupIdentifierInfo tab sym ^. identifierArgsNum

        go' :: Maybe [Node] -> Node -> Maybe [Node]
        go' acc node = case node of
          NApp {} ->
            let (h, args) = unfoldApps' node
             in case h of
                  NIdt Ident {..}
                    | _identSymbol == sym
                        && length args == argsNum ->
                        Just args
                  _ -> acc
          _ -> acc

    substApps :: Symbol -> Node -> Node -> Node
    substApps sym snode = sumap go'
      where
        argsNum = lookupIdentifierInfo tab sym ^. identifierArgsNum

        go' :: Node -> Node
        go' node = case node of
          NApp {} ->
            let (h, args) = unfoldApps' node
             in case h of
                  NIdt Ident {..}
                    | _identSymbol == sym
                        && length args == argsNum ->
                        snode
                  _ -> node
          _ -> node

caseCallLifting :: InfoTable -> InfoTable
caseCallLifting tab = mapAllNodes (convertNode tab) tab
