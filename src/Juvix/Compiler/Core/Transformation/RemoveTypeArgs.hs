module Juvix.Compiler.Core.Transformation.RemoveTypeArgs
  ( removeTypeArgs,
    module Juvix.Compiler.Core.Transformation.Base,
  )
where

import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Transformation.Base

convertNode :: Module -> Node -> Node
convertNode md = convert mempty
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
                | isTypeConstr md ty -> End (mkDynamic _varInfo)
                | otherwise -> End (NVar (shiftVar (-k) v))
        where
          k = length (filter (isTypeConstr md . (^. binderType)) (take _varIndex (toList vars)))
      NIdt Ident {..} ->
        let fi = lookupIdentifierInfo md _identSymbol
         in if
                | isTypeConstr md (fi ^. identifierType) ->
                    Recur (lookupIdentifierNode md _identSymbol)
                | otherwise ->
                    Recur node
      NApp App {..} ->
        let (h, args) = unfoldApps node
            ty =
              case h of
                NVar (Var {..}) ->
                  BL.lookup _varIndex vars ^. binderType
                NIdt (Ident {..}) ->
                  let fi = lookupIdentifierInfo md _identSymbol
                   in fi ^. identifierType
                _ -> unsupported node
            args' = filterArgs snd ty args
         in if
                | isTypeConstr md ty ->
                    End (mkDynamic _appInfo)
                | null args' ->
                    End (convert vars h)
                | otherwise ->
                    End (mkApps (convert vars h) (map (second (convert vars)) args'))
      NCtr (Constr {..}) ->
        let ci = lookupConstructorInfo md _constrTag
            ty = ci ^. constructorType
            args' = filterArgs id ty _constrArgs
         in End (mkConstr _constrInfo _constrTag (map (convert vars) args'))
      NCase (Case {..}) ->
        End (mkCase _caseInfo _caseInductive (convert vars _caseValue) (map convertBranch _caseBranches) (fmap (convert vars) _caseDefault))
        where
          nParams :: Int
          nParams = maybe 0 (length . (^. inductiveParams)) (lookupInductiveInfo' md _caseInductive)
          convertBranch :: CaseBranch -> CaseBranch
          convertBranch br@CaseBranch {..} =
            let paramBinders = map (set binderType mkSmallUniv) (take nParams _caseBranchBinders)
                argBinders = drop nParams _caseBranchBinders
                tyargs = drop nParams (typeArgs (lookupConstructorInfo md _caseBranchTag ^. constructorType))
                argBinders' = zipWith (\b ty -> if isDynamic (b ^. binderType) && isTypeConstr md ty then set binderType ty b else b) argBinders (tyargs ++ repeat mkDynamic')
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
            | isTypeConstr md (b ^. binderType) =
                filterBinders (BL.cons b vars') bs
          filterBinders vars' (b : bs) =
            over binderType (convert vars') b : filterBinders (BL.cons b vars') bs
      NLam (Lambda {..})
        | isTypeConstr md (_lambdaBinder ^. binderType) ->
            End (convert (BL.cons _lambdaBinder vars) _lambdaBody)
      NLet (Let {..})
        | isTypeConstr md (_letItem ^. letItemBinder . binderType) ->
            End (convert (BL.cons (_letItem ^. letItemBinder) vars) _letBody)
      NPi (Pi {..})
        | isTypeConstr md (_piBinder ^. binderType) && not (isTypeConstr md _piBody) ->
            End (convert (BL.cons _piBinder vars) _piBody)
      _ -> Recur node
      where
        filterArgs :: (a -> Node) -> Type -> [a] -> [a]
        filterArgs getNode ty args = case (ty, args) of
          (NPi Pi {..}, arg : args') ->
            let ty' = subst (getNode arg) _piBody
                args'' = filterArgs getNode ty' args'
             in if
                    | isTypeConstr md (_piBinder ^. binderType) ->
                        args''
                    | otherwise ->
                        arg : args''
          _ ->
            args

convertIdent :: Module -> IdentifierInfo -> IdentifierInfo
convertIdent md ii =
  ii
    { _identifierType = ty',
      _identifierArgsNum = length tyargs',
      _identifierArgNames = filterArgNames (ii ^. identifierType) (ii ^. identifierArgNames)
    }
  where
    ty' = convertNode md (ii ^. identifierType)
    tyargs' = typeArgs ty'

    filterArgNames :: Type -> [Maybe Text] -> [Maybe Text]
    filterArgNames ty argnames = case (ty, argnames) of
      (NPi Pi {..}, name : argnames')
        | isTypeConstr md (_piBinder ^. binderType) ->
            filterArgNames _piBody argnames'
        | otherwise ->
            name : filterArgNames _piBody argnames'
      _ ->
        argnames

convertConstructor :: Module -> ConstructorInfo -> ConstructorInfo
convertConstructor md ci =
  ci
    { _constructorType = ty',
      _constructorArgsNum = length (typeArgs ty')
    }
  where
    ty' = convertNode md (ci ^. constructorType)

convertInductive :: Module -> InductiveInfo -> InductiveInfo
convertInductive md ii =
  ii
    { _inductiveKind = ty',
      _inductiveParams = map (over paramKind (convertNode md) . fst) $ filter (not . isTypeConstr md . snd) (zipExact (ii ^. inductiveParams) tyargs)
    }
  where
    tyargs = typeArgs (ii ^. inductiveKind)
    ty' = convertNode md (ii ^. inductiveKind)

convertAxiom :: Module -> AxiomInfo -> AxiomInfo
convertAxiom md = over axiomType (convertNode md)

removeTypeArgs :: Module -> Module
removeTypeArgs md =
  filterOutTypeSynonyms $
    mapAxioms (convertAxiom md) $
      mapInductives (convertInductive md) $
        mapConstructors (convertConstructor md) $
          mapIdents (convertIdent md) $
            mapT (const (convertNode md)) md
