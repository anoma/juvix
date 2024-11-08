module Juvix.Compiler.Core.Transformation.Optimize.CaseCallLifting (caseCallLifting) where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base

convertNode :: Module -> Node -> Node
convertNode md = umap go
  where
    go :: Node -> Node
    go = \case
      NCase Case {..}
        | not (null idents) ->
            if
                | isCaseBoolean _caseBranches && not (isImmediate md _caseValue) ->
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
          (tyargs, tgt) = unfoldPi' (lookupIdentifierInfo md sym ^. identifierType)
          tyargs' = drop (length appArgs) tyargs
          ty = substs appArgs (mkPis' tyargs' tgt)
          brs' = map (\br -> over caseBranchBody (substApps sym (mkVar' (br ^. caseBranchBindersNum + idx))) br) brs
          def' = fmap (substApps sym (mkVar' idx)) def

          computeArgs :: [[Node]] -> Maybe [Node] -> [Node]
          computeArgs args dargs
            | null (head' args) = []
            | otherwise =
                shift
                  (-idx - 1)
                  (mkCase' ind (shift (lvl + 1) val) (zipWithExact (set caseBranchBody) hbs brs) hdef)
                  : computeArgs args' dargs'
            where
              hbs = map head' args
              hdef = fmap head' dargs
              args' = map tail' args
              dargs' = fmap tail' dargs

    gatherIdents :: HashSet Symbol -> Node -> HashSet Symbol
    gatherIdents = sgather go'
      where
        go' :: HashSet Symbol -> Node -> HashSet Symbol
        go' acc node = case node of
          NApp {} ->
            let (h, args) = unfoldApps' node
             in case h of
                  NIdt Ident {..}
                    | length args == lookupIdentifierInfo md _identSymbol ^. identifierArgsNum ->
                        HashSet.insert _identSymbol acc
                  _ -> acc
          _ -> acc

    countApps :: Symbol -> Node -> Int
    countApps sym = sgather go' 0
      where
        argsNum = lookupIdentifierInfo md sym ^. identifierArgsNum

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
        argsNum = lookupIdentifierInfo md sym ^. identifierArgsNum

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
        argsNum = lookupIdentifierInfo md sym ^. identifierArgsNum

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

-- | Lifts calls in case branches out of the case expression, for functions that
-- are called exactly once in each branch.
--
-- Transforms, e.g.,
--
-- case M
-- | C1 x y := R1 (f A1 A2)
-- | C2 z := R2 (f B1 B2)
--
-- to
--
-- let m := M;
--      r := f (case m | C1 x y := A1 | C2 z := B1) (case m | C1 x y := A2 | C2 z := B2);
-- in
-- case m
-- | C1 x y := R1 r
-- | C2 z := R2 r
--
-- This allows to compile more Juvix programs to VampIR by automatically
-- translating a large class of non-linearly-recursive programs into
-- linearly-recursive ones.
--
-- For example,
--
-- def power' : Int → Int → Int → Int :=
--   λ(acc : Int) λ(a : Int) λ(b : Int)
--     if = b 0 then
--       acc
--     else if = (% b 2) 0 then
--       power' acc (* a a) (/ b 2)
--     else
--       power' (* acc a) (* a a) (/ b 2);
--
-- is transformed into
--
-- def power' : Int → Int → Int → Int :=
--   λ(acc : Int) λ(a : Int) λ(b : Int)
--     if = b 0 then
--       acc
--     else
--       let _X : Bool := = (% b 2) 0
--       in power' (if _X then acc else * acc a) (* a a) (/ b 2);
--
-- References:
--  - https://github.com/anoma/juvix/issues/2200
--  - https://github.com/anoma/juvix/pull/2218
caseCallLifting :: Module -> Module
caseCallLifting md = mapAllNodes (convertNode md) md
