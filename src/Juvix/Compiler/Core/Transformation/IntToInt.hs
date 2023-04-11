module Juvix.Compiler.Core.Transformation.IntToInt where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base
import Safe (headMay)

data BuiltinIntCtor
  = BuiltinIntCtorOfNat
  | BuiltinIntCtorNegSuc

convertNode :: InfoTable -> Node -> Node
convertNode tab = rmap go
  where
    go :: ([BinderChange] -> Node -> Node) -> Node -> Node
    go recur node = case node of
      NApp (App _ (NIdt (Ident {..})) l)
        | Just _identSymbol == tab ^. infoIntToInt -> go recur l
      NApp (App _ (NApp (App _ (NIdt (Ident {..})) l)) r) ->
        recur [] $ convertIdentApp node (\g -> g _identInfo l r) _identSymbol
      NApp (App _ (NIdt (Ident {..})) l) ->
        recur [] $ convertSingleArgIdentApp node l _identInfo _identSymbol
      NIdt (Ident {..})
        | Just _identSymbol == tab ^. infoIntToInt ->
            mkLambda' mkTypeInteger' (mkVar' 0)
      NIdt (Ident {..}) ->
        recur [] $ convertSingleArgIdent node _identInfo _identSymbol
      NCtr (Constr {..}) ->
        let ci = fromJust $ HashMap.lookup _constrTag (tab ^. infoConstructors)
         in case ci ^. constructorBuiltin of
              Just BuiltinIntOfNat -> recur [] (fromJust (headMay _constrArgs))
              Just BuiltinIntNegSuc -> recur [] (negSucConv (fromJust (headMay _constrArgs)))
              _ -> recur [] node
      NCase (Case {..}) ->
        let ii = lookupInductiveInfo tab _caseInductive
         in case ii ^. inductiveBuiltin of
              Just (BuiltinTypeInductive BuiltinInt) ->
                case _caseBranches of
                  [br] -> makeIf' br (maybeBranch _caseDefault)
                  [br1, br2] -> case (builtinCtor br1, builtinCtor br2) of
                    (BuiltinIntCtorOfNat, BuiltinIntCtorNegSuc) -> makeIf br1 br2
                    (BuiltinIntCtorNegSuc, BuiltinIntCtorOfNat) -> makeIf br2 br1
                    _ -> impossible
                  [] -> recur [] $ fromJust _caseDefault
                  _ -> impossible
              _ -> recur [] node
        where
          makeIf' :: CaseBranch -> Node -> Node
          makeIf' caseBranch defaultNode =
            let boolSym = lookupConstructorInfo tab (BuiltinTag TagTrue) ^. constructorInductive
                cv = go recur _caseValue
                binder = fromJust (headMay (caseBranch ^. caseBranchBinders))
                binder' = over binderType (go recur) binder
                mkBody n = go (recur . (BCKeep binder :)) n
             in case builtinCtor caseBranch of
                  BuiltinIntCtorOfNat ->
                    mkIf
                      _caseInfo
                      boolSym
                      (mkBuiltinApp' OpIntLe [mkConstant' (ConstInteger 0), cv])
                      (mkLet mempty binder' cv (mkBody (caseBranch ^. caseBranchBody)))
                      (go recur defaultNode)
                  BuiltinIntCtorNegSuc ->
                    mkIf
                      _caseInfo
                      boolSym
                      (mkBuiltinApp' OpIntLt [cv, mkConstant' (ConstInteger 0)])
                      (mkLet mempty binder' (negSucConv cv) (mkBody (caseBranch ^. caseBranchBody)))
                      (go recur defaultNode)

          makeIf :: CaseBranch -> CaseBranch -> Node
          makeIf ofNatBranch negSucBranch =
            let boolSym = lookupConstructorInfo tab (BuiltinTag TagTrue) ^. constructorInductive
                cv = go recur _caseValue
                binder :: CaseBranch -> Binder
                binder br = fromJust (headMay (br ^. caseBranchBinders))
                binder' br = over binderType (go recur) (binder br)
                mkBody br = go (recur . (BCKeep (binder br) :)) (br ^. caseBranchBody)
             in mkIf
                  _caseInfo
                  boolSym
                  (mkBuiltinApp' OpIntLe [mkConstant' (ConstInteger 0), cv])
                  (mkLet mempty (binder' ofNatBranch) cv (mkBody ofNatBranch))
                  (mkLet mempty (binder' negSucBranch) (negSucConv cv) (mkBody negSucBranch))

          builtinCtor :: CaseBranch -> BuiltinIntCtor
          builtinCtor CaseBranch {..} =
            let ci = fromJust $ HashMap.lookup _caseBranchTag (tab ^. infoConstructors)
             in case ci ^. constructorBuiltin of
                  Just BuiltinIntOfNat -> BuiltinIntCtorOfNat
                  Just BuiltinIntNegSuc -> BuiltinIntCtorNegSuc
                  _ -> impossible

          maybeBranch :: Maybe Node -> Node
          maybeBranch = fromMaybe (mkBuiltinApp' OpFail [mkConstant' (ConstString "no matching branch")])
      NTyp TypeConstr {..} ->
        case ii ^. inductiveBuiltin of
          Just (BuiltinTypeInductive BuiltinInt) -> mkTypeInteger'
          _ -> recur [] node
        where
          ii = fromJust $ tab ^. infoInductives . at _typeConstrSymbol
      _ -> recur [] node

    -- Transforms n to -(n+1)
    negSucConv :: Node -> Node
    negSucConv n =
      mkBuiltinApp'
        OpIntSub
        [ mkConstant' (ConstInteger 0),
          mkBuiltinApp' OpIntAdd [n, mkConstant' (ConstInteger 1)]
        ]

    convertIdentApp :: Node -> ((Info -> Node -> Node -> Node) -> Node) -> Symbol -> Node
    convertIdentApp node f sym =
      let ii = fromJust $ HashMap.lookup sym (tab ^. infoIdentifiers)
       in case ii ^. identifierBuiltin of
            Just BuiltinIntEq -> f (\info x y -> mkBuiltinApp info OpEq [x, y])
            Just BuiltinIntPlus -> f (\info x y -> mkBuiltinApp info OpIntAdd [x, y])
            Just BuiltinIntSubNat -> f (\info x y -> mkBuiltinApp info OpIntSub [x, y])
            Just BuiltinIntMul -> f (\info x y -> mkBuiltinApp info OpIntMul [x, y])
            Just BuiltinIntDiv -> f (\info x y -> mkBuiltinApp info OpIntDiv [x, y])
            Just BuiltinIntMod -> f (\info x y -> mkBuiltinApp info OpIntMod [x, y])
            Just BuiltinIntSub -> f (\info x y -> mkBuiltinApp info OpIntSub [x, y])
            _ -> node

    convertSingleArgIdentApp :: Node -> Node -> Info -> Symbol -> Node
    convertSingleArgIdentApp node l info sym =
      let ii = fromJust $ HashMap.lookup sym (tab ^. infoIdentifiers)
          negNode = negNatBody info l
       in case ii ^. identifierBuiltin of
            Just BuiltinIntNegNat -> negNode
            Just BuiltinIntNeg -> negNode
            _ ->
              convertIdentApp
                node
                ( \g ->
                    mkLet' mkTypeInteger' l $
                      mkLambda' mkTypeInteger' $
                        g info (mkVar' 1) (mkVar' 0)
                )
                sym

    convertSingleArgIdent :: Node -> Info -> Symbol -> Node
    convertSingleArgIdent node info sym =
      let ii = fromJust $ HashMap.lookup sym (tab ^. infoIdentifiers)
          negNode = mkLambda' mkTypeInteger' $ negNatBody info (mkVar' 0)
       in case ii ^. identifierBuiltin of
            Just BuiltinIntNegNat -> negNode
            Just BuiltinIntNeg -> negNode
            _ ->
              convertIdentApp
                node
                ( \g ->
                    mkLambda' mkTypeInteger' $
                      mkLambda' mkTypeInteger' $
                        g info (mkVar' 1) (mkVar' 0)
                )
                sym

    negNatBody :: Info -> Node -> Node
    negNatBody info n = mkBuiltinApp info OpIntSub [mkConstant' (ConstInteger 0), n]

filterIntBuiltins :: InfoTable -> InfoTable
filterIntBuiltins tab =
  let tab' =
        over
          infoIdentifiers
          (HashMap.filter (isNotIntBuiltin . (^. identifierBuiltin)))
          tab
   in pruneInfoTable tab'
  where
    isNotIntBuiltin :: Maybe BuiltinFunction -> Bool
    isNotIntBuiltin = \case
      Just b -> not (isIntBuiltin b)
      Nothing -> True

intToInt :: InfoTable -> InfoTable
intToInt tab = filterIntBuiltins $ mapAllNodes (convertNode tab') tab'
  where
    tab' =
      case tab ^. infoIntToInt of
        Just sym ->
          tab {_identContext = HashMap.insert sym (mkLambda' mkTypeInteger' (mkVar' 0)) (tab ^. identContext)}
        Nothing ->
          tab
