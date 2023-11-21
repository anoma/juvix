module Juvix.Compiler.Internal.Extra
  ( module Juvix.Compiler.Internal.Extra,
    module Juvix.Compiler.Internal.Extra.Base,
    module Juvix.Compiler.Internal.Language,
    module Juvix.Compiler.Internal.Extra.Clonable,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.Stream qualified as Stream
import Juvix.Compiler.Internal.Data.InfoTable.Base
import Juvix.Compiler.Internal.Extra.Base
import Juvix.Compiler.Internal.Extra.Clonable
import Juvix.Compiler.Internal.Extra.DependencyBuilder
import Juvix.Compiler.Internal.Language
import Juvix.Prelude

constructorArgTypes :: ConstructorInfo -> ([InductiveParameter], [Expression])
constructorArgTypes i =
  ( i ^. constructorInfoInductiveParameters,
    constructorArgs (i ^. constructorInfoType)
  )

constructorReturnType :: ConstructorInfo -> Expression
constructorReturnType info =
  let inductiveParams = fst (constructorArgTypes info)
      paramNames = inductiveParams ^.. each . inductiveParamName
      ind = ExpressionIden (IdenInductive (info ^. constructorInfoInductive))
      saturatedTy = foldExplicitApplication ind (map (ExpressionIden . IdenVar) paramNames)
   in saturatedTy

fullInductiveType :: InductiveInfo -> Expression
fullInductiveType info =
  let ps = info ^. inductiveInfoDef . inductiveParameters
   in foldr
        (\p k -> p ^. inductiveParamType --> k)
        (info ^. inductiveInfoDef . inductiveType)
        ps

constructorType :: ConstructorInfo -> Expression
constructorType info =
  let (inductiveParams, constrArgs) = constructorArgTypes info
      args =
        map inductiveToFunctionParam inductiveParams
          ++ map unnamedParameter constrArgs
      saturatedTy = constructorReturnType info
   in foldFunType args saturatedTy

inductiveToFunctionParam :: InductiveParameter -> FunctionParameter
inductiveToFunctionParam InductiveParameter {..} =
  FunctionParameter
    { _paramName = Just _inductiveParamName,
      _paramImplicit = Implicit,
      _paramType = _inductiveParamType
    }

constructorImplicity :: ConstructorInfo -> IsImplicit
constructorImplicity info =
  if info ^. constructorInfoTrait then ImplicitInstance else Explicit

patternArgFromVar :: IsImplicit -> VarName -> PatternArg
patternArgFromVar i v =
  PatternArg
    { _patternArgIsImplicit = i,
      _patternArgName = Nothing,
      _patternArgPattern = PatternVariable v
    }

-- | Given `mkPair`, returns (mkPair a b, [a, b])
genConstructorPattern :: (Members '[NameIdGen] r) => Interval -> ConstructorInfo -> Sem r (PatternArg, [VarName])
genConstructorPattern loc info = genConstructorPattern' impl loc (info ^. constructorInfoName) (length (snd (constructorArgTypes info)))
  where
    impl = constructorImplicity info

-- | Given `mkPair`, returns (mkPair a b, [a, b])
genConstructorPattern' :: (Members '[NameIdGen] r) => IsImplicit -> Interval -> Name -> Int -> Sem r (PatternArg, [VarName])
genConstructorPattern' impl loc cname cargs = do
  vars <- mapM (freshVar loc) (Stream.take cargs allWords)
  return (mkConstructorVarPattern impl cname vars, vars)

mkConstructorVarPattern :: IsImplicit -> Name -> [VarName] -> PatternArg
mkConstructorVarPattern impl c vars =
  PatternArg
    { _patternArgIsImplicit = impl,
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
  Maybe BuiltinFunction ->
  ConstructorInfo ->
  Int ->
  Sem r FunctionDef
genFieldProjection _funDefName _funDefBuiltin info fieldIx = do
  body' <- genBody
  let (inductiveParams, constrArgs) = constructorArgTypes info
      implicity = constructorImplicity info
      saturatedTy = unnamedParameter' implicity (constructorReturnType info)
      inductiveArgs = map inductiveToFunctionParam inductiveParams
      retTy = constrArgs !! fieldIx
  return
    FunctionDef
      { _funDefExamples = [],
        _funDefTerminating = False,
        _funDefInstance = False,
        _funDefCoercion = False,
        _funDefArgsInfo = mempty,
        _funDefPragmas = mempty {_pragmasInline = Just InlineAlways},
        _funDefBody = body',
        _funDefType = foldFunType (inductiveArgs ++ [saturatedTy]) retTy,
        _funDefName,
        _funDefBuiltin
      }
  where
    genBody :: Sem r Expression
    genBody = do
      (pat, vars) <- genConstructorPattern (getLoc _funDefName) info
      let body = toExpression (vars !! fieldIx)
          cl =
            LambdaClause
              { _lambdaPatterns = pure pat,
                _lambdaBody = body
              }
      return . ExpressionLambda $
        Lambda
          { _lambdaType = Nothing,
            _lambdaClauses = pure cl
          }

buildLetMutualBlocks ::
  NonEmpty PreLetStatement ->
  NonEmpty (SCC PreLetStatement)
buildLetMutualBlocks ss = nonEmpty' . mapMaybe nameToPreStatement $ scomponents
  where
    -- TODO buildDependencyInfoLet is repeating too much work when there are big nested lets
    depInfo = buildDependencyInfoLet ss

    scomponents :: [SCC Name] = buildSCCs depInfo

    statementsByName :: HashMap Name PreLetStatement
    statementsByName = HashMap.fromList (map mkAssoc (toList ss))
      where
        mkAssoc :: PreLetStatement -> (Name, PreLetStatement)
        mkAssoc s = case s of
          PreLetFunctionDef i -> (i ^. funDefName, s)

    getStmt :: Name -> Maybe PreLetStatement
    getStmt n = statementsByName ^. at n

    nameToPreStatement :: SCC Name -> Maybe (SCC PreLetStatement)
    nameToPreStatement = nonEmptySCC . fmap getStmt
      where
        nonEmptySCC :: SCC (Maybe a) -> Maybe (SCC a)
        nonEmptySCC = \case
          AcyclicSCC a -> AcyclicSCC <$> a
          CyclicSCC p -> CyclicSCC . toList <$> nonEmpty (catMaybes p)

mkLetClauses :: NonEmpty PreLetStatement -> [LetClause]
mkLetClauses pre = goSCC <$> (toList (buildLetMutualBlocks pre))
  where
    goSCC :: SCC PreLetStatement -> LetClause
    goSCC = \case
      AcyclicSCC (PreLetFunctionDef f) -> LetFunDef f
      CyclicSCC fs -> LetMutualBlock (MutualBlockLet fs')
        where
          fs' :: NonEmpty FunctionDef
          fs' = nonEmpty' (map getFun fs)
            where
              getFun :: PreLetStatement -> FunctionDef
              getFun = \case
                PreLetFunctionDef f -> f

inlineLet :: forall r. (Members '[NameIdGen] r) => Let -> Sem r Expression
inlineLet l = do
  (lclauses, subs) <-
    runOutputList
      . execState (mempty @Subs)
      $ forM (l ^. letClauses) helper
  body' <- substitutionE subs (l ^. letExpression)
  return $ case nonEmpty lclauses of
    Nothing -> body'
    Just cl' ->
      ExpressionLet
        Let
          { _letClauses = cl',
            _letExpression = body'
          }
  where
    helper :: forall r'. (r' ~ (State Subs ': Output LetClause ': r)) => LetClause -> Sem r' ()
    helper c = do
      subs <- get
      c' <- substitutionE subs c
      case subsClause c' of
        Nothing -> output c'
        Just (n, b) -> modify' @Subs (set (at n) (Just b))

    subsClause :: LetClause -> Maybe (Name, Expression)
    subsClause = \case
      LetMutualBlock {} -> Nothing
      LetFunDef f -> mkAssoc f
      where
        mkAssoc :: FunctionDef -> Maybe (Name, Expression)
        mkAssoc = \case
          FunctionDef
            { _funDefType = ExpressionHole {},
              _funDefBody = body,
              _funDefName = name,
              _funDefArgsInfo = []
            } -> Just (name, body)
          _ -> Nothing
