module Juvix.Compiler.Core.Transformation.RemoveTypeArgs
  ( removeTypeArgs,
    module Juvix.Compiler.Core.Transformation.Base,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Extra

convertNode :: InfoTable -> Node -> Node
convertNode tab = dmapLR (go 0)
  where
    unsupported :: forall a . a
    unsupported = error "remove type arguments: unsupported node"

    go :: Int -> BinderList Binder -> Node -> Recur
    go k vars node = case node of
      NVar v@(Var {..}) ->
        let ty = BL.lookup _varIndex vars ^. binderType in
          if
            | isTypeConstr ty -> End (mkDynamic _varInfo)
            | otherwise -> End (NVar (shiftVar (-k) v))
      NApp (App {}) ->
        let (h, args) = unfoldApps node
            ty =
              case h of
                NVar (Var {..}) ->
                  BL.lookup _varIndex vars ^. binderType
                NIdt (Ident {..}) ->
                  let fi = fromJust $ HashMap.lookup _identSymbol (tab ^. infoIdentifiers) in
                    fi ^. identifierType
                _ -> unsupported
            args' = filterArgs ty args
        in
        if
          | null args' -> Recur h
          | otherwise -> Recur (mkApps h args')
      NCtr (Constr {..}) ->
        let ci = fromJust $ HashMap.lookup _constrTag (tab ^. infoConstructors)
            ty = ci ^. constructorType
            args' = filterArgs ty _constrArgs
        in
        Recur (mkConstr _constrInfo _constrTag args')
      -- TODO: adjust Case expressions
      NLam (Lambda {..}) | isTypeConstr (_lambdaBinder ^. binderType) ->
        End (dmapLR' (BL.cons _lambdaBinder vars, go (k + 1)) _lambdaBody)
      NPi (Pi {..}) | isTypeConstr (_piBinder ^. binderType) && not (isTypeConstr _piBody) ->
        End (dmapLR' (BL.cons _piBinder vars, go (k + 1)) _piBody)
      _ -> Recur node
      where
        filterArgs :: Type -> [a] -> [a]
        filterArgs ty args =
          map fst $
            filter (not . isTypeConstr . snd) (zip args (typeArgs ty ++ repeat mkDynamic'))

convertIdent :: InfoTable -> IdentifierInfo -> IdentifierInfo
convertIdent tab ii = ii {
    _identifierType = ty',
    _identifierArgsInfo = map (over argumentType (convertNode tab) . fst) $ filter (not . isTypeConstr . snd) (zipExact (ii ^. identifierArgsInfo) tyargs),
    _identifierArgsNum = length (typeArgs ty')
  }
  where
    tyargs = typeArgs (ii ^. identifierType)
    ty' = convertNode tab (ii ^. identifierType)

convertConstructor :: InfoTable -> ConstructorInfo -> ConstructorInfo
convertConstructor tab ci = ci {
    _constructorType = ty',
    _constructorArgsNum = length (typeArgs ty')
  }
  where
     ty' = convertNode tab (ci ^. constructorType)

convertInductive :: InfoTable -> InductiveInfo -> InductiveInfo
convertInductive tab ii = ii {
    _inductiveKind = ty',
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
