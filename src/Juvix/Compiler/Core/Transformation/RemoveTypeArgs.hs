module Juvix.Compiler.Core.Transformation.RemoveTypeArgs
  ( removeTypeArgs,
    module Juvix.Compiler.Core.Transformation.Base,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Transformation.Base

convertNode :: InfoTable -> Node -> Node
convertNode tab = convert mempty
  where
    unsupported :: forall a. Node -> a
    unsupported node = error ("remove type arguments: unsupported node\n\t" <> ppTrace node)

    convert :: BinderList Binder -> Node -> Node
    convert vars = dmapLR' (vars, go)

    go :: BinderList Binder -> Node -> Recur
    go vars node = case node of
      NVar v@(Var {..}) ->
        let ty = BL.lookup _varIndex vars ^. binderType
         in if
                | isTypeConstr tab ty -> End (mkDynamic _varInfo)
                | otherwise -> End (NVar (shiftVar (-k) v))
        where
          k = length (filter (isTypeConstr tab . (^. binderType)) (take _varIndex (toList vars)))
      NIdt Ident {..} ->
        let fi = lookupIdentifierInfo tab _identSymbol
         in if
                | isTypeConstr tab (fi ^. identifierType) ->
                    Recur (lookupIdentifierNode tab _identSymbol)
                | otherwise ->
                    Recur node
      NApp App {..} ->
        let (h, args) = unfoldApps node
            ty =
              case h of
                NVar (Var {..}) ->
                  BL.lookup _varIndex vars ^. binderType
                NIdt (Ident {..}) ->
                  let fi = lookupIdentifierInfo tab _identSymbol
                   in fi ^. identifierType
                _ -> unsupported node
            args' = filterArgs snd ty args
         in if
                | isTypeConstr tab ty ->
                    End (mkDynamic _appInfo)
                | null args' ->
                    End (convert vars h)
                | otherwise ->
                    End (mkApps (convert vars h) (map (second (convert vars)) args'))
      NCtr (Constr {..}) ->
        let ci = lookupConstructorInfo tab _constrTag
            ty = ci ^. constructorType
            args' = filterArgs id ty _constrArgs
         in End (mkConstr _constrInfo _constrTag (map (convert vars) args'))
      NCase (Case {..}) ->
        End (mkCase _caseInfo _caseInductive (convert vars _caseValue) (map convertBranch _caseBranches) (fmap (convert vars) _caseDefault))
        where
          nParams :: Int
          nParams = maybe 0 (length . (^. inductiveParams)) (tab ^. infoInductives . at _caseInductive)
          convertBranch :: CaseBranch -> CaseBranch
          convertBranch br@CaseBranch {..} =
            let paramBinders = map (set binderType mkSmallUniv) (take nParams _caseBranchBinders)
                argBinders = drop nParams _caseBranchBinders
                tyargs = drop nParams (typeArgs (fromJust (tab ^. infoConstructors . at _caseBranchTag) ^. constructorType))
                argBinders' = zipWith (\b ty -> if isDynamic (b ^. binderType) && isTypeConstr tab ty then set binderType ty b else b) argBinders (tyargs ++ repeat mkDynamic')
                binders' =
                  filterBinders
                    (BL.prependRev paramBinders vars)
                    argBinders'
                body' =
                  convert
                    (BL.prependRev argBinders' (BL.prependRev paramBinders vars))
                    _caseBranchBody
             in br
                  { _caseBranchBinders = binders',
                    _caseBranchBindersNum = length binders',
                    _caseBranchBody = body'
                  }
          filterBinders :: BinderList Binder -> [Binder] -> [Binder]
          filterBinders _ [] = []
          filterBinders vars' (b : bs)
            | isTypeConstr tab (b ^. binderType) =
                filterBinders (BL.cons b vars') bs
          filterBinders vars' (b : bs) =
            over binderType (convert vars') b : filterBinders (BL.cons b vars') bs
      NLam (Lambda {..})
        | isTypeConstr tab (_lambdaBinder ^. binderType) ->
            End (convert (BL.cons _lambdaBinder vars) _lambdaBody)
      NLet (Let {..})
        | isTypeConstr tab (_letItem ^. letItemBinder . binderType) ->
            End (convert (BL.cons (_letItem ^. letItemBinder) vars) _letBody)
      NPi (Pi {..})
        | isTypeConstr tab (_piBinder ^. binderType) && not (isTypeConstr tab _piBody) ->
            End (convert (BL.cons _piBinder vars) _piBody)
      _ -> Recur node
      where
        filterArgs :: (a -> Node) -> Type -> [a] -> [a]
        filterArgs getNode ty args = case (ty, args) of
          (NPi Pi {..}, arg : args') ->
            let ty' = subst (getNode arg) _piBody
                args'' = filterArgs getNode ty' args'
             in if
                    | isTypeConstr tab (_piBinder ^. binderType) ->
                        args''
                    | otherwise ->
                        arg : args''
          _ ->
            args

convertIdent :: InfoTable -> IdentifierInfo -> IdentifierInfo
convertIdent tab ii =
  ii
    { _identifierType = ty',
      _identifierArgsNum = length tyargs'
    }
  where
    ty' = convertNode tab (ii ^. identifierType)
    tyargs' = typeArgs ty'

convertConstructor :: InfoTable -> ConstructorInfo -> ConstructorInfo
convertConstructor tab ci =
  ci
    { _constructorType = ty',
      _constructorArgsNum = length (typeArgs ty')
    }
  where
    ty' = convertNode tab (ci ^. constructorType)

convertInductive :: InfoTable -> InductiveInfo -> InductiveInfo
convertInductive tab ii =
  ii
    { _inductiveKind = ty',
      _inductiveParams = map (over paramKind (convertNode tab) . fst) $ filter (not . isTypeConstr tab . snd) (zipExact (ii ^. inductiveParams) tyargs)
    }
  where
    tyargs = typeArgs (ii ^. inductiveKind)
    ty' = convertNode tab (ii ^. inductiveKind)

convertAxiom :: InfoTable -> AxiomInfo -> AxiomInfo
convertAxiom tab = over axiomType (convertNode tab)

filterOutTypeSynonyms :: InfoTable -> InfoTable
filterOutTypeSynonyms tab = pruneInfoTable tab'
  where
    tab' = tab {_infoIdentifiers = idents'}
    idents' = HashMap.filter (\ii -> not (isTypeConstr tab (ii ^. identifierType))) (tab ^. infoIdentifiers)

removeTypeArgs :: InfoTable -> InfoTable
removeTypeArgs tab =
  filterOutTypeSynonyms $
    mapAxioms (convertAxiom tab) $
      mapInductives (convertInductive tab) $
        mapConstructors (convertConstructor tab) $
          mapIdents (convertIdent tab) $
            mapT (const (convertNode tab)) tab
