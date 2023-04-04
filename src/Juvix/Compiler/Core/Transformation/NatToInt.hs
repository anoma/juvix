module Juvix.Compiler.Core.Transformation.NatToInt (natToInt) where

import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.NameInfo
import Juvix.Compiler.Core.Transformation.Base

convertNode :: InfoTable -> Node -> Node
convertNode tab = rmap go
  where
    go :: ([BinderChange] -> Node -> Node) -> Node -> Node
    go recur node = case node of
      NApp (App _ (NIdt (Ident {..})) l)
        | Just _identSymbol == tab ^. infoIntToNat ->
            go recur l
      NApp (App _ (NApp (App _ (NIdt (Ident {..})) l)) r) ->
        recur [] $ convertIdentApp node (\g -> g _identInfo l r) _identSymbol
      NApp (App _ (NIdt (Ident {..})) l) ->
        recur [] $
          convertIdentApp
            node
            ( \g ->
                mkLet' mkTypeInteger' l $
                  mkLambda' mkTypeInteger' $
                    g _identInfo (mkVar' 1) (mkVar' 0)
            )
            _identSymbol
      NIdt (Ident {..})
        | Just _identSymbol == tab ^. infoIntToNat ->
            mkLambda' mkTypeInteger' (mkVar' 0)
      NIdt (Ident {..}) ->
        recur [] $
          convertIdentApp
            node
            ( \g ->
                mkLambda' mkTypeInteger' $
                  mkLambda' mkTypeInteger' $
                    g _identInfo (mkVar' 1) (mkVar' 0)
            )
            _identSymbol
      NCtr (Constr {..}) ->
        let ci = lookupConstructorInfo tab _constrTag
         in case ci ^. constructorBuiltin of
              Just BuiltinNatZero ->
                mkConstant _constrInfo (ConstInteger 0)
              Just BuiltinNatSuc ->
                recur [] $ mkBuiltinApp _constrInfo OpIntAdd (_constrArgs ++ [mkConstant' (ConstInteger 1)])
              _ -> recur [] node
      NCase (Case {..}) ->
        let ii = lookupInductiveInfo tab _caseInductive
         in case ii ^. inductiveBuiltin of
              Just (BuiltinTypeInductive BuiltinNat) ->
                case _caseBranches of
                  [br] -> makeIf br (maybeBranch _caseDefault)
                  [br1, br2] ->
                    if
                        | br1 ^. caseBranchBindersNum == 1 && br2 ^. caseBranchBindersNum == 0 ->
                            makeIf br1 (br2 ^. caseBranchBody)
                        | br2 ^. caseBranchBindersNum == 1 && br1 ^. caseBranchBindersNum == 0 ->
                            makeIf br2 (br1 ^. caseBranchBody)
                        | otherwise ->
                            impossible
                  [] -> recur [] $ fromJust _caseDefault
                  _ -> impossible
              _ -> recur [] node
        where
          makeIf :: CaseBranch -> Node -> Node
          makeIf CaseBranch {..} br =
            let ci = lookupConstructorInfo tab (BuiltinTag TagTrue)
                sym = ci ^. constructorInductive
             in case _caseBranchBindersNum of
                  0 ->
                    recur [] $ mkIf _caseInfo sym (mkBuiltinApp' OpEq [_caseValue, mkConstant' (ConstInteger 0)]) _caseBranchBody br
                  1 ->
                    mkLet mempty binder' (mkBuiltinApp' OpIntSub [go recur _caseValue, mkConstant' (ConstInteger 1)]) $
                      mkIf
                        _caseInfo
                        sym
                        (mkBuiltinApp' OpIntLe [mkConstant' (ConstInteger 0), mkVar (Info.singleton (NameInfo name)) 0])
                        (go (recur . (BCKeep binder :)) _caseBranchBody)
                        (go (recur . (BCAdd 1 :)) br)
                    where
                      binder = List.head _caseBranchBinders
                      name = binder ^. binderName
                      binder' = over binderType (go recur) binder
                  _ -> impossible
          maybeBranch :: Maybe Node -> Node
          maybeBranch = fromMaybe (mkBuiltinApp' OpFail [mkConstant' (ConstString "no matching branch")])
      NTyp TypeConstr {..} ->
        case ii ^. inductiveBuiltin of
          Just (BuiltinTypeInductive BuiltinNat) -> mkTypeInteger'
          _ -> recur [] node
        where
          ii = fromJust $ tab ^. infoInductives . at _typeConstrSymbol
      _ -> recur [] node

    convertIdentApp :: Node -> ((Info -> Node -> Node -> Node) -> Node) -> Symbol -> Node
    convertIdentApp node f sym =
      let ii = lookupIdentifierInfo tab sym
       in case ii ^. identifierBuiltin of
            Just BuiltinNatPlus -> f (\info x y -> mkBuiltinApp info OpIntAdd [x, y])
            Just BuiltinNatSub ->
              f
                ( \info x y ->
                    mkLet' mkTypeInteger' (mkBuiltinApp info OpIntSub [x, y]) $
                      mkIf'
                        boolSymbol
                        (mkBuiltinApp' OpIntLe [mkConstant' (ConstInteger 0), mkVar' 0])
                        (mkVar' 0)
                        (mkConstant' (ConstInteger 0))
                )
              where
                boolSymbol =
                  lookupConstructorInfo tab (BuiltinTag TagTrue) ^. constructorInductive
            Just BuiltinNatMul -> f (\info x y -> mkBuiltinApp info OpIntMul [x, y])
            Just BuiltinNatUDiv ->
              f
                ( \info x y ->
                    mkBuiltinApp info OpIntDiv [mkBuiltinApp' OpIntAdd [x, mkBuiltinApp' OpIntSub [y, mkConstant' (ConstInteger 1)]], y]
                )
            Just BuiltinNatDiv -> f (\info x y -> mkBuiltinApp info OpIntDiv [x, y])
            Just BuiltinNatMod -> f (\info x y -> mkBuiltinApp info OpIntMod [x, y])
            Just BuiltinNatLe -> f (\info x y -> mkBuiltinApp info OpIntLe [x, y])
            Just BuiltinNatLt -> f (\info x y -> mkBuiltinApp info OpIntLt [x, y])
            Just BuiltinNatEq -> f (\info x y -> mkBuiltinApp info OpEq [x, y])
            _ -> node

filterNatBuiltins :: InfoTable -> InfoTable
filterNatBuiltins tab =
  let tab' =
        over
          infoIdentifiers
          (HashMap.filter (isNotNatBuiltin . (^. identifierBuiltin)))
          tab
   in pruneInfoTable tab'
  where
    isNotNatBuiltin :: Maybe BuiltinFunction -> Bool
    isNotNatBuiltin = \case
      Just b -> not (isNatBuiltin b)
      Nothing -> True

natToInt :: InfoTable -> InfoTable
natToInt tab = filterNatBuiltins $ mapAllNodes (convertNode tab') tab'
  where
    tab' =
      case tab ^. infoIntToNat of
        Just sym ->
          tab {_identContext = HashMap.insert sym (mkLambda' mkTypeInteger' (mkVar' 0)) (tab ^. identContext)}
        Nothing ->
          tab
