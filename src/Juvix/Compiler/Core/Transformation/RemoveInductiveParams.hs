module Juvix.Compiler.Core.Transformation.RemoveInductiveParams
  ( removeInductiveParams,
    module Juvix.Compiler.Core.Transformation.Base,
  )
where

import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base

convertNode :: Module -> Node -> Node
convertNode md node0 = rmap go node0
  where
    go :: ([BinderChange] -> Node -> Node) -> Node -> Node
    go recur node = case node of
      NTyp typeConstr -> NTyp $ set typeConstrArgs [] typeConstr
      NCtr Constr {..} ->
        let ci = lookupConstructorInfo md _constrTag
            nParams = lookupParamsNum md (ci ^. constructorInductive)
         in mkConstr _constrInfo _constrTag (map (go recur) (drop nParams _constrArgs))
      NCase Case {..} ->
        mkCase _caseInfo _caseInductive (go recur _caseValue) (map convertBranch _caseBranches) (fmap (go recur) _caseDefault)
        where
          nParams = lookupParamsNum md _caseInductive

          convertBranch :: CaseBranch -> CaseBranch
          convertBranch br@CaseBranch {..} =
            let tyargs = typeArgs (lookupConstructorInfo md _caseBranchTag ^. constructorType)
                binders = zipWith (\b ty -> if isDynamic (b ^. binderType) && isTypeConstr md ty then set binderType ty b else b) _caseBranchBinders (tyargs ++ repeat mkDynamic')
                bcs = map (\b -> mkBCRemove b mkDynamic') $ take nParams binders
                (binders', bcs') = filterBinders bcs (drop nParams binders)
                body' = go (recur . (bcs' ++)) _caseBranchBody
             in br
                  { _caseBranchBinders = binders',
                    _caseBranchBindersNum = length binders',
                    _caseBranchBody = body'
                  }

          filterBinders :: [BinderChange] -> [Binder] -> ([Binder], [BinderChange])
          filterBinders bcs = \case
            [] -> ([], bcs)
            b : bs -> (over binderType (go (recur . (bcs ++))) b : bs', bcs')
              where
                (bs', bcs') = filterBinders (bcs ++ [BCKeep b]) bs
      _ -> recur [] node

convertIdent :: Module -> IdentifierInfo -> IdentifierInfo
convertIdent md ii =
  ii
    { _identifierType = ty'
    }
  where
    ty' = convertNode md (ii ^. identifierType)

convertConstructor :: Module -> ConstructorInfo -> ConstructorInfo
convertConstructor md ci =
  ci
    { _constructorType = ty',
      _constructorArgsNum = length (typeArgs ty')
    }
  where
    nParams = lookupParamsNum md (ci ^. constructorInductive)
    tyargs' = drop nParams (typeArgs (ci ^. constructorType))
    ty' = convertNode md (mkPis' tyargs' (typeTarget (ci ^. constructorType)))

convertInductive :: Module -> InductiveInfo -> InductiveInfo
convertInductive md ii =
  ii
    { _inductiveKind = ty',
      _inductiveParams = []
    }
  where
    ty' = convertNode md (typeTarget (ii ^. inductiveKind))

-- | Removes parameters of inductive types from constructors and
-- case-expressions.
removeInductiveParams :: Module -> Module
removeInductiveParams md =
  mapInductives (convertInductive md)
    . mapConstructors (convertConstructor md)
    . mapIdents (convertIdent md)
    $ mapT (const (convertNode md)) md
