module Juvix.Compiler.Core.Transformation.NatToInt(natToInt) where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base

convertNode :: InfoTable -> Node -> Node
convertNode tab = dmap go
  where
    go :: Node -> Node
    go node = case node of
      NApp (App _ (NApp (App _ (NIdt (Ident {..})) l)) r) ->
        convertIdentApp node (\op -> mkBuiltinApp _identInfo op [l, r]) _identSymbol
      NApp (App _ (NIdt (Ident {..})) l) ->
        convertIdentApp node (\op -> mkLambdaTy mkTypeInteger' $ mkBuiltinApp _identInfo op [l, mkVar' 0]) _identSymbol
      NIdt (Ident {..}) ->
        convertIdentApp
          node
          (\op ->
            mkLambdaTy mkTypeInteger' $
            mkLambdaTy mkTypeInteger' $
            mkBuiltinApp _identInfo op [mkVar' 1, mkVar' 0])
          _identSymbol
      NCtr (Constr {..}) ->
        let ci = fromJust $ HashMap.lookup _constrTag (tab ^. infoConstructors)
        in
        case ci ^. constructorBuiltin of
          Just BuiltinNatZero ->
            mkConstant _constrInfo (ConstInteger 0)
          Just BuiltinNatSuc ->
            mkBuiltinApp _constrInfo OpIntAdd (_constrArgs ++ [mkConstant' (ConstInteger 1)])
          _ -> node
--  TODO:    NCase (Case {..}) -> undefined
      _ -> node

    convertIdentApp :: Node -> (BuiltinOp -> Node) -> Symbol -> Node
    convertIdentApp node f sym =
      let ii = fromJust $ HashMap.lookup sym (tab ^. infoIdentifiers)
      in
      case ii ^. identifierBuiltin of
        Just BuiltinNatPlus -> f OpIntAdd
        _ -> node

natToInt :: InfoTable -> InfoTable
natToInt tab = mapT (const (convertNode tab)) tab
