module Juvix.Compiler.Core.Transformation.NatToInt (natToInt) where

import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.NameInfo
import Juvix.Compiler.Core.Transformation.Base

convertNode :: InfoTable -> Node -> Node
convertNode tab = convert [] 0
  where
    -- `levels` is the list of de Bruijn levels at which binders are inserted
    convert :: [Level] -> Level -> Node -> Node
    convert levels bl node = dmapCNR' (bl, go) levels node

    go :: [Level] -> Level -> Node -> Recur' [Level]
    go levels bl node = case node of
      NVar (Var {..}) ->
        End' (mkVar _varInfo (_varIndex + length (filter (\x -> x >= bl - _varIndex) levels)))
      NApp (App _ (NIdt (Ident {..})) l)
        | Just _identSymbol == tab ^. infoIntToNat ->
            End' (convert levels bl l)
      NApp (App _ (NApp (App _ (NIdt (Ident {..})) l)) r) ->
        Recur' (levels, convertIdentApp node (\g -> g _identInfo l r) _identSymbol)
      NApp (App _ (NIdt (Ident {..})) l) ->
        Recur'
          ( levels,
            convertIdentApp
              node
              ( \g ->
                  mkLet' mkTypeInteger' l $
                    mkLambda' mkTypeInteger' $
                      g _identInfo (mkVar' 1) (mkVar' 0)
              )
              _identSymbol
          )
      NIdt (Ident {..})
        | Just _identSymbol == tab ^. infoIntToNat ->
            End' (mkLambda' mkTypeInteger' (mkVar' 0))
      NIdt (Ident {..}) ->
        Recur'
          ( levels,
            convertIdentApp
              node
              ( \g ->
                  mkLambda' mkTypeInteger' $
                    mkLambda' mkTypeInteger' $
                      g _identInfo (mkVar' 1) (mkVar' 0)
              )
              _identSymbol
          )
      NCtr (Constr {..}) ->
        let ci = fromJust $ HashMap.lookup _constrTag (tab ^. infoConstructors)
         in case ci ^. constructorBuiltin of
              Just BuiltinNatZero ->
                Recur' (levels, mkConstant _constrInfo (ConstInteger 0))
              Just BuiltinNatSuc ->
                Recur' (levels, mkBuiltinApp _constrInfo OpIntAdd (_constrArgs ++ [mkConstant' (ConstInteger 1)]))
              _ -> Recur' (levels, node)
      NCase (Case {..}) ->
        let ii = fromJust $ HashMap.lookup _caseInductive (tab ^. infoInductives)
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
                  [] -> Recur' (levels, fromJust _caseDefault)
                  _ -> impossible
              _ -> Recur' (levels, node)
        where
          makeIf :: CaseBranch -> Node -> Recur' [Level]
          makeIf CaseBranch {..} br =
            let ci = fromJust $ HashMap.lookup (BuiltinTag TagTrue) (tab ^. infoConstructors)
                sym = ci ^. constructorInductive
             in case _caseBranchBindersNum of
                  0 ->
                    Recur' (levels, mkIf _caseInfo sym (mkBuiltinApp' OpEq [_caseValue, mkConstant' (ConstInteger 0)]) _caseBranchBody br)
                  1 ->
                    End' $
                      mkLet mempty (emptyBinder {_binderName = name}) (mkBuiltinApp' OpIntSub [convert levels bl _caseValue, mkConstant' (ConstInteger 1)]) $
                        mkIf
                          _caseInfo
                          sym
                          (mkBuiltinApp' OpIntLe [mkConstant' (ConstInteger 0), mkVar (Info.singleton (NameInfo name)) 0])
                          (convert levels (bl + 1) _caseBranchBody)
                          (convert (bl : levels) bl br)
                    where
                      name = List.head _caseBranchBinders ^. binderName
                  _ -> impossible
          maybeBranch :: Maybe Node -> Node
          maybeBranch = fromMaybe (mkBuiltinApp' OpFail [mkConstant' (ConstString "no matching branch")])
      _ -> Recur' (levels, node)

    convertIdentApp :: Node -> ((Info -> Node -> Node -> Node) -> Node) -> Symbol -> Node
    convertIdentApp node f sym =
      let ii = fromJust $ HashMap.lookup sym (tab ^. infoIdentifiers)
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
                  fromJust (HashMap.lookup (BuiltinTag TagTrue) (tab ^. infoConstructors)) ^. constructorInductive
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

natToInt :: InfoTable -> InfoTable
natToInt tab = mapT (const (convertNode tab')) tab'
  where
    tab' =
      case tab ^. infoIntToNat of
        Just sym ->
          tab {_identContext = HashMap.insert sym (mkLambda' mkTypeInteger' (mkVar' 0)) (tab ^. identContext)}
        Nothing ->
          tab
