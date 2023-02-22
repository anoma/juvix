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

isTypeConstr :: Type -> Bool
isTypeConstr ty = case typeTarget ty of
  NUniv {} -> True
  _ -> False

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
                | isTypeConstr ty -> End (mkDynamic _varInfo)
                | otherwise -> End (NVar (shiftVar (-k) v))
        where
          k = length (filter (isTypeConstr . (^. binderType)) (take _varIndex (toList vars)))
      NApp (App {}) ->
        let (h, args) = unfoldApps node
            ty =
              case h of
                NVar (Var {..}) ->
                  BL.lookup _varIndex vars ^. binderType
                NIdt (Ident {..}) ->
                  let fi = fromJust $ HashMap.lookup _identSymbol (tab ^. infoIdentifiers)
                   in fi ^. identifierType
                _ -> unsupported node
            args' = filterArgs ty args
         in if
                | null args' ->
                    End (convert vars h)
                | otherwise ->
                    End (mkApps (convert vars h) (map (second (convert vars)) args'))
      NCtr (Constr {..}) ->
        let ci = fromJust $ HashMap.lookup _constrTag (tab ^. infoConstructors)
            ty = ci ^. constructorType
            args' = filterArgs ty _constrArgs
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
                binders' =
                  filterBinders
                    (BL.prependRev paramBinders vars)
                    argBinders
                body' =
                  convert
                    (BL.prependRev argBinders (BL.prependRev paramBinders vars))
                    _caseBranchBody
             in br
                  { _caseBranchBinders = binders',
                    _caseBranchBindersNum = length binders',
                    _caseBranchBody = body'
                  }
          filterBinders :: BinderList Binder -> [Binder] -> [Binder]
          filterBinders _ [] = []
          filterBinders vars' (b : bs)
            | isTypeConstr (b ^. binderType) =
                filterBinders (BL.cons b vars') bs
          filterBinders vars' (b : bs) =
            over binderType (convert vars') b : filterBinders (BL.cons b vars') bs
      NLam (Lambda {..})
        | isTypeConstr (_lambdaBinder ^. binderType) ->
            End (convert (BL.cons _lambdaBinder vars) _lambdaBody)
      NLet (Let {..})
        | isTypeConstr (_letItem ^. letItemBinder . binderType) ->
            End (convert (BL.cons (_letItem ^. letItemBinder) vars) _letBody)
      NPi (Pi {..})
        | isTypeConstr (_piBinder ^. binderType) && not (isTypeConstr _piBody) ->
            End (convert (BL.cons _piBinder vars) _piBody)
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
      _identifierArgsInfo =
        map (uncurry (set argumentType)) $
          zipExact tyargs' $
            map fst $
              filter (not . isTypeConstr . snd) (zipExact (ii ^. identifierArgsInfo) tyargs),
      _identifierArgsNum = length (typeArgs ty')
    }
  where
    tyargs = typeArgs (ii ^. identifierType)
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
