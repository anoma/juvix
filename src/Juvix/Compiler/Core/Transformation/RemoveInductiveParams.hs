module Juvix.Compiler.Core.Transformation.RemoveInductiveParams
  ( removeInductiveParams,
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
    convert :: BinderList Binder -> Node -> Node
    convert vars = dmapLR' (vars, go)

    go :: BinderList Binder -> Node -> Recur
    go vars node = case node of
      NTyp typeConstr -> End . NTyp $ set typeConstrArgs [] typeConstr
      NCtr Constr {..} ->
        let ci = lookupConstructorInfo md _constrTag
            ii = lookupInductiveInfo md (ci ^. constructorInductive)
            nParams = length (ii ^. inductiveParams)
         in End $
              mkConstr _constrInfo _constrTag (map (convert vars) (drop nParams _constrArgs))
      NCase Case {..} ->
        End $ mkCase _caseInfo _caseInductive (convert vars _caseValue) (map convertBranch _caseBranches) (fmap (convert vars) _caseDefault)
        where
          ii = lookupInductiveInfo md _caseInductive
          nParams = length (ii ^. inductiveParams)

          convertBranch :: CaseBranch -> CaseBranch
          convertBranch br@CaseBranch {..} =
            let tyargs = typeArgs (lookupConstructorInfo md _caseBranchTag ^. constructorType)
                binders = zipWith (\b ty -> if isDynamic (b ^. binderType) && isTypeConstr md ty then set binderType ty b else b) _caseBranchBinders (tyargs ++ repeat mkDynamic')
                binders' = filterBinders vars binders
                body' =
                  convert
                    (BL.prependRev binders vars)
                    _caseBranchBody
             in br
                  { _caseBranchBinders = binders',
                    _caseBranchBindersNum = length binders',
                    _caseBranchBody = body'
                  }

          filterBinders :: BinderList Binder -> [Binder] -> [Binder]
          filterBinders vars' = \case
            [] -> []
            b : bs
              | isTypeConstr md (b ^. binderType) ->
                  filterBinders (BL.cons b vars') bs
              | otherwise -> over binderType (convert vars') b : filterBinders (BL.cons b vars') bs
      _ -> Recur node

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

-- | Removes parameters of inductive types from constructors.
removeInductiveParams :: Module -> Module
removeInductiveParams md =
  mapInductives (convertInductive md)
    . mapConstructors (convertConstructor md)
    $ mapT (const (convertNode md)) md
