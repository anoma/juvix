module Juvix.Compiler.Core.Transformation.NatToInt (natToInt) where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base

convertNode :: InfoTable -> Node -> Node
convertNode tab = convert [] 0
  where
    convert :: [Level] -> Level -> Node -> Node
    convert levels bl = dmapCNR' (bl, go) levels

    go :: [Level] -> Level -> Node -> Recur' [Level]
    go levels bl node = case node of
      NVar (Var {..}) ->
        End' (mkVar _varInfo (_varIndex + length (filter (\x -> x >= bl - _varIndex) levels)))
      NApp (App _ (NApp (App _ (NIdt (Ident {..})) l)) r) ->
        Recur' (levels, convertIdentApp node (\op -> mkBuiltinApp _identInfo op [l, r]) _identSymbol)
      NApp (App _ (NIdt (Ident {..})) l) ->
        Recur' (levels, convertIdentApp node (\op -> mkLambdaTy mkTypeInteger' $ mkBuiltinApp _identInfo op [l, mkVar' 0]) _identSymbol)
      NIdt (Ident {..}) ->
        Recur'
          ( levels,
            convertIdentApp
              node
              ( \op ->
                  mkLambdaTy mkTypeInteger' $
                    mkLambdaTy mkTypeInteger' $
                      mkBuiltinApp _identInfo op [mkVar' 1, mkVar' 0]
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
              Just BuiltinBool ->
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
                      mkLet' (mkBuiltinApp' OpIntSub [convert levels bl _caseValue, mkConstant' (ConstInteger 1)]) $
                        mkIf
                          _caseInfo
                          sym
                          (mkBuiltinApp' OpIntLe [mkConstant' (ConstInteger 0), mkVar' 0])
                          (convert levels (bl + 1) _caseBranchBody)
                          (convert (bl : levels) bl br)
                  _ -> impossible
          maybeBranch :: Maybe Node -> Node
          maybeBranch = fromMaybe (mkBuiltinApp' OpFail [mkConstant' (ConstString "no matching branch")])
      _ -> Recur' (levels, node)

    convertIdentApp :: Node -> (BuiltinOp -> Node) -> Symbol -> Node
    convertIdentApp node f sym =
      let ii = fromJust $ HashMap.lookup sym (tab ^. infoIdentifiers)
       in case ii ^. identifierBuiltin of
            Just BuiltinNatPlus -> f OpIntAdd
            _ -> node

natToInt :: InfoTable -> InfoTable
natToInt tab = mapT (const (convertNode tab)) tab
