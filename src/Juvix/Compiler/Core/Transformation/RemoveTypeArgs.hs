module Juvix.Compiler.Core.Transformation.RemoveTypeArgs
  ( removeTypeArgs,
    module Juvix.Compiler.Core.Transformation.Base,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base

convertNode :: InfoTable -> Node -> Node
convertNode tab = convert 0 mempty
  where
    unsupported :: forall a. a
    unsupported = error "remove type arguments: unsupported node"

    convert :: Int -> BinderList Binder -> Node -> Node
    convert k vars = dmapLR' (vars, go k)

    go :: Int -> BinderList Binder -> Node -> Recur
    go k vars node = case node of
      NVar v@(Var {..}) ->
        let ty = BL.lookup _varIndex vars ^. binderType
         in if
                | isTypeConstr ty -> End (mkDynamic _varInfo)
                | otherwise -> End (NVar (shiftVar (-k) v))
      NApp (App {}) ->
        let (h, args) = unfoldApps node
            ty =
              case h of
                NVar (Var {..}) ->
                  BL.lookup _varIndex vars ^. binderType
                NIdt (Ident {..}) ->
                  let fi = fromJust $ HashMap.lookup _identSymbol (tab ^. infoIdentifiers)
                   in fi ^. identifierType
                _ -> unsupported
            args' = filterArgs ty args
         in if
                | null args' ->
                  End (convert k vars h)
                | otherwise ->
                  End (mkApps (convert k vars h) (map (second (convert k vars)) args'))
      NCtr (Constr {..}) ->
        let ci = fromJust $ HashMap.lookup _constrTag (tab ^. infoConstructors)
            ty = ci ^. constructorType
            args' = filterArgs ty _constrArgs
         in End (mkConstr _constrInfo _constrTag (map (convert k vars) args'))
      NCase (Case {..}) ->
        End (mkCase _caseInfo (convert k vars _caseValue) (map convertBranch _caseBranches) (fmap (convert k vars) _caseDefault))
        where
          convertBranch :: CaseBranch -> CaseBranch
          convertBranch br@CaseBranch {..} =
            let binders' = filterBinders k vars _caseBranchBinders
                body' = convert
                          (k + _caseBranchBindersNum - length binders')
                          (BL.prependRev _caseBranchBinders vars)
                          _caseBranchBody
            in
              br
                {
                  _caseBranchBinders = binders',
                  _caseBranchBindersNum = length binders',
                  _caseBranchBody = body'
                }
          filterBinders :: Int -> BinderList Binder -> [Binder] -> [Binder]
          filterBinders _ _ [] = []
          filterBinders k' vars' (b : bs) | isTypeConstr (b ^. binderType) =
            filterBinders (k' + 1) (BL.cons b vars') bs
          filterBinders k' vars' (b : bs) =
            over binderType (convert k' vars') b : filterBinders k' (BL.cons b vars') bs
      NLam (Lambda {..})
        | isTypeConstr (_lambdaBinder ^. binderType) ->
            End (convert (k + 1) (BL.cons _lambdaBinder vars) _lambdaBody)
      NPi (Pi {..})
        | isTypeConstr (_piBinder ^. binderType) && not (isTypeConstr _piBody) ->
            End (convert (k + 1) (BL.cons _piBinder vars) _piBody)
      _ -> Recur node
      where
        filterArgs :: Type -> [a] -> [a]
        filterArgs ty args =
          map fst $
            filter (not . isTypeConstr . snd) (zip args (typeArgs ty ++ repeat mkDynamic'))

convertIdent :: InfoTable -> IdentifierInfo -> IdentifierInfo
convertIdent tab ii =
  ii
    { _identifierType = ty',
      _identifierArgsInfo = map (over argumentType (convertNode tab) . fst) $ filter (not . isTypeConstr . snd) (zipExact (ii ^. identifierArgsInfo) tyargs),
      _identifierArgsNum = length (typeArgs ty')
    }
  where
    tyargs = typeArgs (ii ^. identifierType)
    ty' = convertNode tab (ii ^. identifierType)

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
      _inductiveParams = map (over paramKind (convertNode tab) . fst) $ filter (not . isTypeConstr . snd) (zipExact (ii ^. inductiveParams) tyargs),
      _inductiveConstructors = map (convertConstructor tab) (ii ^. inductiveConstructors)
    }
  where
    tyargs = typeArgs (ii ^. inductiveKind)
    ty' = convertNode tab (ii ^. inductiveKind)

convertAxiom :: InfoTable -> AxiomInfo -> AxiomInfo
convertAxiom tab = over axiomType (convertNode tab)

removeTypeArgs :: InfoTable -> InfoTable
removeTypeArgs tab =
  mapAxioms (convertAxiom tab) $
    mapInductives (convertInductive tab) $
      mapConstructors (convertConstructor tab) $
        mapIdents (convertIdent tab) $
          mapT (const (convertNode tab)) tab
