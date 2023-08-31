module Juvix.Compiler.Internal.Extra
  ( module Juvix.Compiler.Internal.Extra,
    module Juvix.Compiler.Internal.Extra.Base,
    module Juvix.Compiler.Internal.Language,
  )
where

import Data.Stream qualified as Stream
import Juvix.Compiler.Internal.Data.InfoTable.Base
import Juvix.Compiler.Internal.Extra.Base
import Juvix.Compiler.Internal.Language
import Juvix.Prelude

constructorArgTypes :: ConstructorInfo -> ([VarName], [Expression])
constructorArgTypes i =
  ( map (^. inductiveParamName) (i ^. constructorInfoInductiveParameters),
    constructorArgs (i ^. constructorInfoType)
  )

constructorReturnType :: ConstructorInfo -> Expression
constructorReturnType info =
  let inductiveParams = fst (constructorArgTypes info)
      ind = ExpressionIden (IdenInductive (info ^. constructorInfoInductive))
      saturatedTy = foldExplicitApplication ind (map (ExpressionIden . IdenVar) inductiveParams)
   in saturatedTy

constructorType :: ConstructorInfo -> Expression
constructorType info =
  let (inductiveParams, constrArgs) = constructorArgTypes info
      args =
        map (typeAbstraction Implicit) inductiveParams
          ++ map unnamedParameter constrArgs
      saturatedTy = constructorReturnType info
   in foldFunType args saturatedTy

patternArgFromVar :: IsImplicit -> VarName -> PatternArg
patternArgFromVar i v =
  PatternArg
    { _patternArgIsImplicit = i,
      _patternArgName = Nothing,
      _patternArgPattern = PatternVariable v
    }

-- | Given `mkPair`, returns (mkPair a b, [a, b])
genConstructorPattern :: (Members '[NameIdGen] r) => Interval -> ConstructorInfo -> Sem r (PatternArg, [VarName])
genConstructorPattern loc info = genConstructorPattern' loc (info ^. constructorInfoName) (length (snd (constructorArgTypes info)))

-- | Given `mkPair`, returns (mkPair a b, [a, b])
genConstructorPattern' :: (Members '[NameIdGen] r) => Interval -> Name -> Int -> Sem r (PatternArg, [VarName])
genConstructorPattern' loc cname cargs = do
  vars <- mapM (freshVar loc) (Stream.take cargs allWords)
  return (mkConstructorVarPattern cname vars, vars)

mkConstructorVarPattern :: Name -> [VarName] -> PatternArg
mkConstructorVarPattern c vars =
  PatternArg
    { _patternArgIsImplicit = Explicit,
      _patternArgName = Nothing,
      _patternArgPattern =
        PatternConstructorApp
          ConstructorApp
            { _constrAppConstructor = c,
              _constrAppType = Nothing,
              _constrAppParameters = map (patternArgFromVar Explicit) vars
            }
    }

-- | Assumes the constructor does not have implicit arguments (which is not
-- allowed at the moment).
genFieldProjection ::
  forall r.
  (Members '[NameIdGen] r) =>
  FunctionName ->
  ConstructorInfo ->
  Int ->
  Sem r FunctionDef
genFieldProjection _funDefName info fieldIx = do
  clause <- genClause
  let (inductiveParams, constrArgs) = constructorArgTypes info
      saturatedTy = unnamedParameter (constructorReturnType info)
      inductiveArgs = map (typeAbstraction Implicit) inductiveParams
      retTy = constrArgs !! fieldIx
  return
    FunctionDef
      { _funDefExamples = [],
        _funDefTerminating = False,
        _funDefInstance = False,
        _funDefBuiltin = Nothing,
        _funDefPragmas = mempty,
        _funDefClauses = pure clause,
        _funDefType = foldFunType (inductiveArgs ++ [saturatedTy]) retTy,
        ..
      }
  where
    genClause :: Sem r FunctionClause
    genClause = do
      (pat, vars) <- genConstructorPattern (getLoc _funDefName) info
      let body = toExpression (vars !! fieldIx)
      return
        FunctionClause
          { _clauseName = _funDefName,
            _clausePatterns = [pat],
            _clauseBody = body
          }
