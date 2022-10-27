module Juvix.Compiler.Internal.Translation.FromAbstract
  ( module Juvix.Compiler.Internal.Translation.FromAbstract.Data.Context,
    module Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination,
    TranslationState,
    iniState,
    fromAbstract,
  )
where

import Data.Graph
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Abstract.Data.NameDependencyInfo qualified as Abstract
import Juvix.Compiler.Abstract.Extra.DependencyBuilder
import Juvix.Compiler.Abstract.Extra.DependencyBuilder qualified as Abstract
import Juvix.Compiler.Abstract.Language qualified as Abstract
import Juvix.Compiler.Abstract.Translation.FromConcrete.Data.Context qualified as Abstract
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination hiding (Graph)
import Juvix.Compiler.Internal.Translation.FromAbstract.Data.Context
import Juvix.Compiler.Pipeline.EntryPoint qualified as E
import Juvix.Prelude

newtype TranslationState = TranslationState
  { -- | Top modules are supposed to be included at most once.
    _translationStateIncluded :: HashSet Abstract.TopModuleName
  }

iniState :: TranslationState
iniState =
  TranslationState
    { _translationStateIncluded = mempty
    }

makeLenses ''TranslationState

fromAbstract ::
  Members '[Error JuvixError] r =>
  Abstract.AbstractResult ->
  Sem r InternalResult
fromAbstract abstractResults = do
  unless
    noTerminationOption
    ( mapError
        (JuvixError @TerminationError)
        (checkTermination topModule infoTable)
    )
  let abstractModules = abstractResults ^. Abstract.resultModules
      exportsTbl = abstractResults ^. Abstract.resultExports
  _resultModules' <-
    runReader exportsTbl $
      evalState
        iniState
        ( mapM
            goModule
            abstractModules
        )
  return
    InternalResult
      { _resultAbstract = abstractResults,
        _resultModules = _resultModules',
        _resultDepInfo = depInfo
      }
  where
    topModule = head (abstractResults ^. Abstract.resultModules)
    infoTable = abstractResults ^. Abstract.resultTable
    noTerminationOption =
      abstractResults
        ^. Abstract.abstractResultEntryPoint
        . E.entryPointNoTermination
    depInfo = buildDependencyInfo (abstractResults ^. Abstract.resultModules) (abstractResults ^. Abstract.resultExports)

goModule ::
  Members '[Reader ExportsTable, State TranslationState] r =>
  Abstract.TopModule ->
  Sem r Module
goModule m = do
  expTbl <- ask
  let mutualBlocks :: [NonEmpty Abstract.FunctionDef]
      mutualBlocks = buildMutualBlocks expTbl
  _moduleBody' <- goModuleBody mutualBlocks (m ^. Abstract.moduleBody)
  examples' <- mapM goExample (m ^. Abstract.moduleExamples)
  return
    Module
      { _moduleName = m ^. Abstract.moduleName,
        _moduleExamples = examples',
        _moduleBody = _moduleBody'
      }
  where
    funsByName :: HashMap Abstract.FunctionName Abstract.FunctionDef
    funsByName =
      HashMap.fromList
        [ (d ^. Abstract.funDefName, d)
          | Abstract.StatementFunction d <- m ^. Abstract.moduleBody . Abstract.moduleStatements
        ]
    getFun :: Abstract.FunctionName -> Maybe Abstract.FunctionDef
    getFun n = funsByName ^. at n
    buildMutualBlocks :: Abstract.ExportsTable -> [NonEmpty Abstract.FunctionDef]
    buildMutualBlocks expTbl = mapMaybe (nonEmpty . mapMaybe getFun . toList . fromNonEmptyTree) scomponents
      where
        fromNonEmptyTree :: Tree a -> NonEmpty a
        fromNonEmptyTree = fromJust . nonEmpty . toList
        depInfo :: Abstract.NameDependencyInfo
        depInfo = Abstract.buildDependencyInfo (pure m) expTbl
        graph :: Graph
        graph = Abstract.buildDependencyInfo (pure m) expTbl ^. Abstract.depInfoGraph
        scomponents :: [Tree Abstract.Name]
        scomponents = fmap (Abstract.nameFromVertex depInfo) <$> scc graph

unsupported :: Text -> a
unsupported thing = error ("Abstract to Internal: Not yet supported: " <> thing)

goModuleBody ::
  Members '[Reader ExportsTable, State TranslationState] r =>
  [NonEmpty Abstract.FunctionDef] ->
  Abstract.ModuleBody ->
  Sem r ModuleBody
goModuleBody mutualBlocks b = do
  mutualBlocks' <- mapM (fmap (StatementFunction . MutualBlock) . mapM goFunctionDef) mutualBlocks
  statements' <- mapMaybeM goStatement (b ^. Abstract.moduleStatements)
  return
    ModuleBody
      { _moduleStatements = statements' <> mutualBlocks'
      }

goImport :: Members '[Reader ExportsTable, State TranslationState] r => Abstract.TopModule -> Sem r (Maybe Include)
goImport m = do
  inc <- gets (HashSet.member (m ^. Abstract.moduleName) . (^. translationStateIncluded))
  if
      | inc -> return Nothing
      | otherwise -> do
          modify (over translationStateIncluded (HashSet.insert (m ^. Abstract.moduleName)))
          m' <- goModule m
          return
            ( Just
                Include
                  { _includeModule = m'
                  }
            )

goStatement ::
  Members '[Reader ExportsTable, State TranslationState] r =>
  Abstract.Statement ->
  Sem r (Maybe Statement)
goStatement = \case
  Abstract.StatementAxiom d -> Just . StatementAxiom <$> goAxiomDef d
  Abstract.StatementForeign f -> return (Just (StatementForeign f))
  Abstract.StatementFunction {} -> return Nothing
  Abstract.StatementImport i -> fmap StatementInclude <$> goImport i
  Abstract.StatementLocalModule {} -> unsupported "local modules"
  Abstract.StatementInductive i -> Just . StatementInductive <$> goInductiveDef i

goTypeIden :: Abstract.Iden -> Iden
goTypeIden = \case
  Abstract.IdenFunction f -> IdenFunction (f ^. Abstract.functionRefName)
  Abstract.IdenConstructor {} -> unsupported "constructors in types"
  Abstract.IdenVar v -> IdenVar v
  Abstract.IdenInductive d -> IdenInductive (d ^. Abstract.inductiveRefName)
  Abstract.IdenAxiom a -> IdenAxiom (a ^. Abstract.axiomRefName)

goAxiomDef :: Abstract.AxiomDef -> Sem r AxiomDef
goAxiomDef a = do
  _axiomType' <- goType (a ^. Abstract.axiomType)
  return
    AxiomDef
      { _axiomName = a ^. Abstract.axiomName,
        _axiomBuiltin = a ^. Abstract.axiomBuiltin,
        _axiomType = _axiomType'
      }

goFunctionParameter :: Abstract.FunctionParameter -> Sem r FunctionParameter
goFunctionParameter f = case f ^. Abstract.paramName of
  Just var
    | isSmallType (f ^. Abstract.paramType) && isOmegaUsage (f ^. Abstract.paramUsage) ->
        return (FunctionParameter (Just var) (f ^. Abstract.paramImplicit) (smallUniverseE (getLoc var)))
    | otherwise -> unsupported "named function arguments only for small types without usages"
  Nothing
    | isOmegaUsage (f ^. Abstract.paramUsage) -> unnamedParameter <$> goType (f ^. Abstract.paramType)
    | otherwise -> unsupported "usages"

isOmegaUsage :: Usage -> Bool
isOmegaUsage u = case u of
  UsageOmega -> True
  _ -> False

goFunction :: Abstract.Function -> Sem r Function
goFunction (Abstract.Function l r) = do
  l' <- goFunctionParameter l
  r' <- goType r
  return (Function l' r')

goFunctionDef :: Abstract.FunctionDef -> Sem r FunctionDef
goFunctionDef f = do
  _funDefClauses' <- mapM (goFunctionClause _funDefName') (f ^. Abstract.funDefClauses)
  _funDefType' <- goType (f ^. Abstract.funDefTypeSig)
  _funDefExamples' <- mapM goExample (f ^. Abstract.funDefExamples)
  return
    FunctionDef
      { _funDefName = _funDefName',
        _funDefType = _funDefType',
        _funDefClauses = _funDefClauses',
        _funDefExamples = _funDefExamples',
        _funDefBuiltin = f ^. Abstract.funDefBuiltin
      }
  where
    _funDefName' :: Name
    _funDefName' = f ^. Abstract.funDefName

goExample :: Abstract.Example -> Sem r Example
goExample e = do
  e' <- goExpression (e ^. Abstract.exampleExpression)
  return
    Example
      { _exampleExpression = e',
        _exampleId = e ^. Abstract.exampleId
      }

goFunctionClause :: Name -> Abstract.FunctionClause -> Sem r FunctionClause
goFunctionClause n c = do
  _clauseBody' <- goExpression (c ^. Abstract.clauseBody)
  _clausePatterns' <- mapM goPatternArg (c ^. Abstract.clausePatterns)
  return
    FunctionClause
      { _clauseName = n,
        _clausePatterns = _clausePatterns',
        _clauseBody = _clauseBody'
      }

goPatternArg :: Abstract.PatternArg -> Sem r PatternArg
goPatternArg p = do
  pat' <- goPattern (p ^. Abstract.patternArgPattern)
  return
    PatternArg
      { _patternArgIsImplicit = p ^. Abstract.patternArgIsImplicit,
        _patternArgName = p ^. Abstract.patternArgName,
        _patternArgPattern = pat'
      }

goPattern :: Abstract.Pattern -> Sem r Pattern
goPattern p = case p of
  Abstract.PatternVariable v -> return (PatternVariable v)
  Abstract.PatternConstructorApp c -> PatternConstructorApp <$> goConstructorApp c
  Abstract.PatternWildcard i -> return (PatternWildcard i)
  Abstract.PatternEmpty -> unsupported "pattern empty"

goConstructorApp :: Abstract.ConstructorApp -> Sem r ConstructorApp
goConstructorApp c = do
  _constrAppParameters' <- mapM goPatternArg (c ^. Abstract.constrAppParameters)
  return
    ConstructorApp
      { _constrAppConstructor = c ^. Abstract.constrAppConstructor . Abstract.constructorRefName,
        _constrAppParameters = _constrAppParameters'
      }

isSmallType :: Abstract.Expression -> Bool
isSmallType e = case e of
  Abstract.ExpressionUniverse u -> isSmallUni u
  _ -> False

isSmallUni :: Universe -> Bool
isSmallUni u = 0 == fromMaybe 0 (u ^. universeLevel)

goUniverse :: Universe -> SmallUniverse
goUniverse u
  | isSmallUni u = SmallUniverse (getLoc u)
  | otherwise = unsupported "big universes"

goType :: Abstract.Expression -> Sem r Expression
goType e = case e of
  Abstract.ExpressionIden i -> return (ExpressionIden (goTypeIden i))
  Abstract.ExpressionUniverse u -> return (ExpressionUniverse (goUniverse u))
  Abstract.ExpressionApplication a -> ExpressionApplication <$> goTypeApplication a
  Abstract.ExpressionFunction f -> ExpressionFunction <$> goFunction f
  Abstract.ExpressionLiteral {} -> unsupported "literals in types"
  Abstract.ExpressionHole h -> return (ExpressionHole h)
  Abstract.ExpressionLambda {} -> unsupported "lambda in types"

goLambda :: forall r. Abstract.Lambda -> Sem r Lambda
goLambda (Abstract.Lambda cl) = case nonEmpty cl of
  Nothing -> unsupported "empty lambda"
  Just cl' -> Lambda <$> mapM goClause cl'
  where
    goClause :: Abstract.LambdaClause -> Sem r LambdaClause
    goClause (Abstract.LambdaClause ps b) = do
      ps' <- mapM (goPatternArg . explicit) ps
      b' <- goExpression b
      return (LambdaClause ps' b')
      where
        explicit :: Abstract.PatternArg -> Abstract.PatternArg
        explicit p = case p ^. Abstract.patternArgIsImplicit of
          Explicit -> p
          Implicit -> unsupported "implicit patterns in lambda"

goApplication :: Abstract.Application -> Sem r Application
goApplication (Abstract.Application f x i) = do
  f' <- goExpression f
  x' <- goExpression x
  return (Application f' x' i)

goIden :: Abstract.Iden -> Iden
goIden i = case i of
  Abstract.IdenFunction n -> IdenFunction (n ^. Abstract.functionRefName)
  Abstract.IdenConstructor c -> IdenConstructor (c ^. Abstract.constructorRefName)
  Abstract.IdenVar v -> IdenVar v
  Abstract.IdenAxiom a -> IdenAxiom (a ^. Abstract.axiomRefName)
  Abstract.IdenInductive a -> IdenInductive (a ^. Abstract.inductiveRefName)

goExpressionFunction :: forall r. Abstract.Function -> Sem r Function
goExpressionFunction f = do
  l' <- goParam (f ^. Abstract.funParameter)
  r' <- goExpression (f ^. Abstract.funReturn)
  return (Function l' r')
  where
    goParam :: Abstract.FunctionParameter -> Sem r FunctionParameter
    goParam p
      | isOmegaUsage (p ^. Abstract.paramUsage) = do
          ty' <- goExpression (p ^. Abstract.paramType)
          return (FunctionParameter (p ^. Abstract.paramName) (p ^. Abstract.paramImplicit) ty')
      | otherwise = unsupported "usages"

goExpression :: Abstract.Expression -> Sem r Expression
goExpression e = case e of
  Abstract.ExpressionIden i -> return (ExpressionIden (goIden i))
  Abstract.ExpressionUniverse u -> return (ExpressionUniverse (goUniverse u))
  Abstract.ExpressionFunction f -> ExpressionFunction <$> goExpressionFunction f
  Abstract.ExpressionApplication a -> ExpressionApplication <$> goApplication a
  Abstract.ExpressionLambda l -> ExpressionLambda <$> goLambda l
  Abstract.ExpressionLiteral l -> return (ExpressionLiteral l)
  Abstract.ExpressionHole h -> return (ExpressionHole h)

goInductiveParameter :: Abstract.FunctionParameter -> Sem r InductiveParameter
goInductiveParameter f =
  case (f ^. Abstract.paramName, f ^. Abstract.paramUsage, f ^. Abstract.paramType) of
    (Just var, UsageOmega, Abstract.ExpressionUniverse u)
      | isSmallUni u ->
          return
            InductiveParameter
              { _inductiveParamName = var
              }
    (Just {}, _, _) -> unsupported "only type variables of small types are allowed"
    (Nothing, _, _) -> unsupported "unnamed inductive parameters"

goInductiveDef ::
  Abstract.InductiveDef ->
  Sem r InductiveDef
goInductiveDef i =
  if
      | not (isSmallType (i ^. Abstract.inductiveType)) -> unsupported "inductive indices"
      | otherwise -> do
          inductiveParameters' <- mapM goInductiveParameter (i ^. Abstract.inductiveParameters)
          let indTypeName = i ^. Abstract.inductiveName
          inductiveConstructors' <-
            mapM
              goConstructorDef
              (i ^. Abstract.inductiveConstructors)
          examples' <- mapM goExample (i ^. Abstract.inductiveExamples)
          return
            InductiveDef
              { _inductiveName = indTypeName,
                _inductiveParameters = inductiveParameters',
                _inductiveBuiltin = i ^. Abstract.inductiveBuiltin,
                _inductiveConstructors = inductiveConstructors',
                _inductiveExamples = examples',
                _inductivePositive = i ^. Abstract.inductivePositive
              }
  where
    goConstructorDef :: Abstract.InductiveConstructorDef -> Sem r InductiveConstructorDef
    goConstructorDef c = do
      (cParams, cReturnType) <- viewConstructorType (c ^. Abstract.constructorType)
      examples' <- mapM goExample (c ^. Abstract.constructorExamples)
      return
        InductiveConstructorDef
          { _inductiveConstructorName = c ^. Abstract.constructorName,
            _inductiveConstructorParameters = cParams,
            _inductiveConstructorExamples = examples',
            _inductiveConstructorReturnType = cReturnType
          }

goTypeApplication :: Abstract.Application -> Sem r Application
goTypeApplication (Abstract.Application l r i) = do
  l' <- goType l
  r' <- goType r
  return (Application l' r' i)

viewConstructorType :: Abstract.Expression -> Sem r ([Expression], Expression)
viewConstructorType = \case
  Abstract.ExpressionFunction f -> first toList <$> viewFunctionType f
  Abstract.ExpressionIden i -> return ([], ExpressionIden (goTypeIden i))
  Abstract.ExpressionHole h -> return ([], ExpressionHole h)
  Abstract.ExpressionApplication a -> do
    a' <- goTypeApplication a
    return ([], ExpressionApplication a')
  Abstract.ExpressionUniverse u -> return ([], smallUniverseE (getLoc u))
  Abstract.ExpressionLiteral {} -> unsupported "literal in a type"
  Abstract.ExpressionLambda {} -> unsupported "lambda in a constructor type"
  where
    viewFunctionType :: Abstract.Function -> Sem r (NonEmpty Expression, Expression)
    viewFunctionType (Abstract.Function p r) = do
      (args, ret) <- viewConstructorType r
      p' <- goFunctionParameter p
      return $ case p' ^. paramName of
        Just {} -> unsupported "named argument in constructor type"
        Nothing -> (p' ^. paramType :| args, ret)
