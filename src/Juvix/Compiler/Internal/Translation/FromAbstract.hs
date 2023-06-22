module Juvix.Compiler.Internal.Translation.FromAbstract
  ( module Juvix.Compiler.Internal.Translation.FromAbstract.Data.Context,
    fromAbstract,
    fromAbstractExpression,
    fromAbstractInclude,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Abstract.Data.NameDependencyInfo
import Juvix.Compiler.Abstract.Extra.DependencyBuilder
import Juvix.Compiler.Abstract.Extra.DependencyBuilder qualified as Abstract
import Juvix.Compiler.Abstract.Extra.Functions qualified as Abstract
import Juvix.Compiler.Abstract.Language qualified as Abstract
import Juvix.Compiler.Abstract.Translation.FromConcrete.Data.Context qualified as Abstract
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Pretty.Base
import Juvix.Compiler.Internal.Translation.FromAbstract.Data.Context
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker
import Juvix.Compiler.Pipeline.EntryPoint qualified as E
import Juvix.Prelude

data PreStatement
  = PreFunctionDef FunctionDef
  | PreInductiveDef InductiveDef
  | PreAxiomDef AxiomDef

fromAbstract ::
  Members '[Error JuvixError, NameIdGen] r =>
  Abstract.AbstractResult ->
  Sem r InternalResult
fromAbstract abstractResults = do
  let abstractModules = abstractResults ^. Abstract.resultModules
      exportsTbl :: HashSet NameId = abstractResults ^. Abstract.resultExports
  _resultModules' <-
    runReader exportsTbl $
      mapM goModule abstractModules
  let topModule = head _resultModules'
      tbl = buildTable _resultModules'
  unless
    noTerminationOption
    ( mapError
        (JuvixError @TerminationError)
        (checkTermination tbl topModule)
    )
  return
    InternalResult
      { _resultAbstract = abstractResults,
        _resultModules = _resultModules',
        _resultDepInfo = depInfo
      }
  where
    noTerminationOption =
      abstractResults
        ^. Abstract.abstractResultEntryPoint
          . E.entryPointNoTermination
    depInfo = buildDependencyInfo (abstractResults ^. Abstract.resultModules) (abstractResults ^. Abstract.resultExports)

fromAbstractExpression :: Members '[NameIdGen] r => Abstract.Expression -> Sem r Expression
fromAbstractExpression e = runReader depInfo (goExpression e)
  where
    depInfo :: NameDependencyInfo
    depInfo = buildDependencyInfoExpr e

fromAbstractInclude ::
  Members '[Reader ExportsTable, NameIdGen] r =>
  Abstract.Include ->
  Sem r Include
fromAbstractInclude = goInclude

goInclude ::
  Members '[Reader ExportsTable, NameIdGen] r =>
  Abstract.Include ->
  Sem r Include
goInclude (Abstract.Include m) = Include <$> goModule m

goModule ::
  Members '[Reader ExportsTable, NameIdGen] r =>
  Abstract.TopModule ->
  Sem r Module
goModule m = do
  expTbl <- ask
  let depInfo :: NameDependencyInfo
      depInfo = Abstract.buildDependencyInfo (pure m) expTbl
  runReader depInfo $ do
    _moduleBody' <- goModuleBody (m ^. Abstract.moduleBody)
    examples' <- mapM goExample (m ^. Abstract.moduleExamples)
    return
      Module
        { _moduleName = m ^. Abstract.moduleName,
          _moduleExamples = examples',
          _moduleBody = _moduleBody',
          _modulePragmas = m ^. Abstract.modulePragmas
        }

buildLetMutualBlocks ::
  Members '[Reader NameDependencyInfo] r =>
  [FunctionDef] ->
  Sem r [SCC FunctionDef]
buildLetMutualBlocks = fmap (map (fmap fromStmt)) . buildMutualBlocks . map PreFunctionDef
  where
    fromStmt :: PreStatement -> FunctionDef
    fromStmt = \case
      PreFunctionDef f -> f
      _ -> impossible

-- | `StatementInclude`s are no included in the result
buildMutualBlocks ::
  Members '[Reader NameDependencyInfo] r =>
  [PreStatement] ->
  Sem r [SCC PreStatement]
buildMutualBlocks ss = do
  depInfo <- ask
  let scomponents :: [SCC Abstract.Name] = buildSCCs depInfo
  return (boolHack (mapMaybe nameToPreStatement scomponents))
  where
    -- If the builtin bool definition is found, it is moved at the front.
    --
    -- This is a hack needed to translate BuiltinStringToNat in
    -- internal-to-core. BuiltinStringToNat is the only function that depends on
    -- Bool implicitly (i.e. without mentioning it in its type). Eventually
    -- BuiltinStringToNat needs to be removed and so this hack.
    boolHack :: [SCC PreStatement] -> [SCC PreStatement]
    boolHack s = case popFirstJust isBuiltinBool s of
      (Nothing, _) -> s
      (Just boolDef, rest) -> AcyclicSCC (PreInductiveDef boolDef) : rest
      where
        isBuiltinBool :: SCC PreStatement -> Maybe InductiveDef
        isBuiltinBool = \case
          CyclicSCC [PreInductiveDef b]
            | Just BuiltinBool <- b ^. inductiveBuiltin -> Just b
          _ -> Nothing

    statementsByName :: HashMap Abstract.Name PreStatement
    statementsByName = HashMap.fromList (map mkAssoc ss)
      where
        mkAssoc :: PreStatement -> (Abstract.Name, PreStatement)
        mkAssoc s = case s of
          PreInductiveDef i -> (i ^. inductiveName, s)
          PreFunctionDef i -> (i ^. funDefName, s)
          PreAxiomDef i -> (i ^. axiomName, s)

    getStmt :: Abstract.Name -> Maybe PreStatement
    getStmt n = statementsByName ^. at n

    nameToPreStatement :: SCC Abstract.Name -> Maybe (SCC PreStatement)
    nameToPreStatement = nonEmptySCC . fmap getStmt
      where
        nonEmptySCC :: SCC (Maybe a) -> Maybe (SCC a)
        nonEmptySCC = \case
          AcyclicSCC a -> AcyclicSCC <$> a
          CyclicSCC p -> CyclicSCC . toList <$> nonEmpty (catMaybes p)

unsupported :: Text -> a
unsupported thing = error ("Abstract to Internal: Not yet supported: " <> thing)

-- | Note that it ignores import statements
goDefinition ::
  forall r.
  Members '[Reader ExportsTable, Reader NameDependencyInfo, NameIdGen] r =>
  Abstract.Statement ->
  Sem r [PreStatement]
goDefinition = \case
  Abstract.StatementLocalModule m -> concatMapM goDefinition (m ^. Abstract.moduleBody . Abstract.moduleStatements)
  Abstract.StatementInductive i -> pure . PreInductiveDef <$> goInductiveDef i
  Abstract.StatementFunction i -> pure . PreFunctionDef <$> goFunctionDef i
  Abstract.StatementAxiom a -> pure . PreAxiomDef <$> goAxiomDef a
  Abstract.StatementInclude {} -> return []

scanImports :: Abstract.ModuleBody -> [Abstract.Include]
scanImports (Abstract.ModuleBody stmts) = mconcatMap go stmts
  where
    go :: Abstract.Statement -> [Abstract.Include]
    go = \case
      Abstract.StatementLocalModule m -> scanImports (m ^. Abstract.moduleBody)
      Abstract.StatementInclude t -> [t]
      Abstract.StatementInductive {} -> []
      Abstract.StatementFunction {} -> []
      Abstract.StatementAxiom {} -> []

goModuleBody ::
  forall r.
  Members '[Reader ExportsTable, Reader NameDependencyInfo, NameIdGen] r =>
  Abstract.ModuleBody ->
  Sem r ModuleBody
goModuleBody b@(Abstract.ModuleBody stmts) = do
  preDefs <- concatMapM goDefinition stmts
  sccs <- buildMutualBlocks preDefs
  let imports :: [Abstract.Include] = scanImports b
      statements' = map goSCC sccs
  imports' <- map StatementInclude <$> mapM goInclude imports
  return
    ModuleBody
      { _moduleStatements = imports' <> statements'
      }
  where
    goSCC :: SCC PreStatement -> Statement
    goSCC = \case
      AcyclicSCC s -> goAcyclic s
      CyclicSCC c -> goCyclic (nonEmpty' c)
      where
        goCyclic :: NonEmpty PreStatement -> Statement
        goCyclic c = StatementMutual (MutualBlock (goMutual <$> c))
          where
            goMutual :: PreStatement -> MutualStatement
            goMutual = \case
              PreInductiveDef i -> StatementInductive i
              PreFunctionDef i -> StatementFunction i
              _ -> impossible

        goAcyclic :: PreStatement -> Statement
        goAcyclic = \case
          PreInductiveDef i -> one (StatementInductive i)
          PreFunctionDef i -> one (StatementFunction i)
          PreAxiomDef i -> StatementAxiom i
          where
            one :: MutualStatement -> Statement
            one = StatementMutual . MutualBlock . pure

goTypeIden :: Abstract.Iden -> Iden
goTypeIden = \case
  Abstract.IdenFunction f -> IdenFunction f
  Abstract.IdenConstructor {} -> unsupported "constructors in types"
  Abstract.IdenVar v -> IdenVar v
  Abstract.IdenInductive d -> IdenInductive d
  Abstract.IdenAxiom a -> IdenAxiom a

goAxiomDef :: Abstract.AxiomDef -> Sem r AxiomDef
goAxiomDef a = do
  _axiomType' <- goType (a ^. Abstract.axiomType)
  return
    AxiomDef
      { _axiomName = a ^. Abstract.axiomName,
        _axiomBuiltin = a ^. Abstract.axiomBuiltin,
        _axiomType = _axiomType',
        _axiomPragmas = a ^. Abstract.axiomPragmas
      }

goFunctionParameter :: Abstract.FunctionParameter -> Sem r FunctionParameter
goFunctionParameter f = do
  _paramType <- goType (f ^. Abstract.paramType)
  return
    FunctionParameter
      { _paramName = f ^. Abstract.paramName,
        _paramImplicit = f ^. Abstract.paramImplicit,
        _paramType
      }

goFunction :: Abstract.Function -> Sem r Function
goFunction (Abstract.Function l r) = do
  l' <- goFunctionParameter l
  r' <- goType r
  return (Function l' r')

goFunctionDef :: Members '[NameIdGen, Reader NameDependencyInfo] r => Abstract.FunctionDef -> Sem r FunctionDef
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
        _funDefBuiltin = f ^. Abstract.funDefBuiltin,
        _funDefPragmas = f ^. Abstract.funDefPragmas,
        _funDefTerminating = f ^. Abstract.funDefTerminating
      }
  where
    _funDefName' :: Name
    _funDefName' = f ^. Abstract.funDefName

goExample :: Members '[NameIdGen, Reader NameDependencyInfo] r => Abstract.Example -> Sem r Example
goExample e = do
  e' <- goExpression (e ^. Abstract.exampleExpression)
  return
    Example
      { _exampleExpression = e',
        _exampleId = e ^. Abstract.exampleId
      }

goFunctionClause :: Members '[NameIdGen, Reader NameDependencyInfo] r => Name -> Abstract.FunctionClause -> Sem r FunctionClause
goFunctionClause n c = do
  _clauseBody' <- goExpression (c ^. Abstract.clauseBody)
  _clausePatterns' <- mapM goPatternArg (c ^. Abstract.clausePatterns)
  return
    FunctionClause
      { _clauseName = n,
        _clausePatterns = _clausePatterns',
        _clauseBody = _clauseBody'
      }

goPatternArg :: (Members '[NameIdGen] r) => Abstract.PatternArg -> Sem r PatternArg
goPatternArg p = do
  pat' <- goPattern (p ^. Abstract.patternArgPattern)
  return
    PatternArg
      { _patternArgIsImplicit = p ^. Abstract.patternArgIsImplicit,
        _patternArgName = p ^. Abstract.patternArgName,
        _patternArgPattern = pat'
      }

goPattern :: (Members '[NameIdGen] r) => Abstract.Pattern -> Sem r Pattern
goPattern p = case p of
  Abstract.PatternVariable v -> return (PatternVariable v)
  Abstract.PatternConstructorApp c -> PatternConstructorApp <$> goConstructorApp c
  Abstract.PatternWildcard w -> PatternVariable <$> varFromWildcard w
  Abstract.PatternEmpty -> unsupported "pattern empty"

goConstructorApp :: (Members '[NameIdGen] r) => Abstract.ConstructorApp -> Sem r ConstructorApp
goConstructorApp c = do
  _constrAppParameters' <- mapM goPatternArg (c ^. Abstract.constrAppParameters)
  return
    ConstructorApp
      { _constrAppConstructor = c ^. Abstract.constrAppConstructor,
        _constrAppParameters = _constrAppParameters',
        _constrAppType = Nothing
      }

goType :: Abstract.Expression -> Sem r Expression
goType e = case e of
  Abstract.ExpressionIden i -> return (ExpressionIden (goTypeIden i))
  Abstract.ExpressionUniverse u -> return (ExpressionUniverse u)
  Abstract.ExpressionApplication a -> ExpressionApplication <$> goTypeApplication a
  Abstract.ExpressionFunction f -> ExpressionFunction <$> goFunction f
  Abstract.ExpressionLiteral {} -> unsupported "literals in types"
  Abstract.ExpressionHole h -> return (ExpressionHole h)
  Abstract.ExpressionLambda {} -> unsupported "lambda in types"
  Abstract.ExpressionLet {} -> unsupported "let in types"
  Abstract.ExpressionCase {} -> unsupported "case in types"

goLambda :: forall r. Members '[NameIdGen, Reader NameDependencyInfo] r => Abstract.Lambda -> Sem r Lambda
goLambda (Abstract.Lambda cl') = do
  _lambdaClauses <- mapM goClause cl'
  let _lambdaType :: Maybe Expression = Nothing
  return Lambda {..}
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

goApplication :: Members '[NameIdGen, Reader NameDependencyInfo] r => Abstract.Application -> Sem r Application
goApplication (Abstract.Application f x i) = do
  f' <- goExpression f
  x' <- goExpression x
  return (Application f' x' i)

goIden :: Abstract.Iden -> Iden
goIden i = case i of
  Abstract.IdenFunction n -> IdenFunction n
  Abstract.IdenConstructor c -> IdenConstructor c
  Abstract.IdenVar v -> IdenVar v
  Abstract.IdenAxiom a -> IdenAxiom a
  Abstract.IdenInductive a -> IdenInductive a

goExpressionFunction :: forall r. Members '[NameIdGen, Reader NameDependencyInfo] r => Abstract.Function -> Sem r Function
goExpressionFunction f = do
  l' <- goParam (f ^. Abstract.funParameter)
  r' <- goExpression (f ^. Abstract.funReturn)
  return (Function l' r')
  where
    goParam :: Abstract.FunctionParameter -> Sem r FunctionParameter
    goParam p = do
      ty' <- goExpression (p ^. Abstract.paramType)
      return (FunctionParameter (p ^. Abstract.paramName) (p ^. Abstract.paramImplicit) ty')

goExpression :: Members '[NameIdGen, Reader NameDependencyInfo] r => Abstract.Expression -> Sem r Expression
goExpression e = case e of
  Abstract.ExpressionIden i -> return (ExpressionIden (goIden i))
  Abstract.ExpressionUniverse u -> return (ExpressionUniverse u)
  Abstract.ExpressionFunction f -> ExpressionFunction <$> goExpressionFunction f
  Abstract.ExpressionApplication a -> ExpressionApplication <$> goApplication a
  Abstract.ExpressionLambda l -> ExpressionLambda <$> goLambda l
  Abstract.ExpressionLiteral l -> return (ExpressionLiteral (goLiteral l))
  Abstract.ExpressionHole h -> return (ExpressionHole h)
  Abstract.ExpressionLet l -> ExpressionLet <$> goLet l
  Abstract.ExpressionCase c -> ExpressionCase <$> goCase c

goLiteral :: Abstract.LiteralLoc -> LiteralLoc
goLiteral = fmap go
  where
    go :: Abstract.Literal -> Literal
    go = \case
      Abstract.LitString s -> LitString s
      Abstract.LitInteger i -> LitInteger i

goCase :: Members '[NameIdGen, Reader NameDependencyInfo] r => Abstract.Case -> Sem r Case
goCase c = do
  _caseExpression <- goExpression (c ^. Abstract.caseExpression)
  _caseBranches <- mapM goCaseBranch (c ^. Abstract.caseBranches)
  let _caseParens = c ^. Abstract.caseParens
      _caseExpressionType :: Maybe Expression = Nothing
      _caseExpressionWholeType :: Maybe Expression = Nothing
  return Case {..}

goCaseBranch :: Members '[NameIdGen, Reader NameDependencyInfo] r => Abstract.CaseBranch -> Sem r CaseBranch
goCaseBranch b = do
  _caseBranchPattern <- goPatternArg (b ^. Abstract.caseBranchPattern)
  _caseBranchExpression <- goExpression (b ^. Abstract.caseBranchExpression)
  return CaseBranch {..}

goLet :: forall r. (Members '[NameIdGen, Reader NameDependencyInfo] r) => Abstract.Let -> Sem r Let
goLet l = do
  _letExpression <- goExpression (l ^. Abstract.letExpression)
  mutualBlocks <- mapM goFunctionDef funDefs >>= buildLetMutualBlocks
  let _letClauses = nonEmpty' (map goLetClauses mutualBlocks)
  return Let {..}
  where
    funDefs :: [Abstract.FunctionDef]
    funDefs = [f | Abstract.LetFunDef f <- toList (l ^. Abstract.letClauses)]
    goLetClauses :: SCC FunctionDef -> LetClause
    goLetClauses = \case
      AcyclicSCC f -> LetFunDef f
      CyclicSCC m -> LetMutualBlock (MutualBlockLet (nonEmpty' m))

goInductiveParameter :: Abstract.InductiveParameter -> InductiveParameter
goInductiveParameter f =
  InductiveParameter
    { _inductiveParamName = f ^. Abstract.inductiveParamName
    }

goInductiveDef :: forall r. Members '[NameIdGen, Reader NameDependencyInfo] r => Abstract.InductiveDef -> Sem r InductiveDef
goInductiveDef i
  | not (Abstract.isSmallUniverse' (i ^. Abstract.inductiveType)) = unsupported "inductive indices"
  | otherwise = do
      let inductiveParameters' = map goInductiveParameter (i ^. Abstract.inductiveParameters)
          indTypeName = i ^. Abstract.inductiveName
      inductiveConstructors' <-
        mapM
          goConstructorDef
          (i ^. Abstract.inductiveConstructors)
      examples' <- mapM goExample (i ^. Abstract.inductiveExamples)
      ty' <- goExpression (i ^. Abstract.inductiveType)
      return
        InductiveDef
          { _inductiveName = indTypeName,
            _inductiveParameters = inductiveParameters',
            _inductiveBuiltin = i ^. Abstract.inductiveBuiltin,
            _inductiveConstructors = inductiveConstructors',
            _inductiveExamples = examples',
            _inductiveType = ty',
            _inductivePositive = i ^. Abstract.inductivePositive,
            _inductivePragmas = i ^. Abstract.inductivePragmas
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
            _inductiveConstructorReturnType = cReturnType,
            _inductiveConstructorPragmas = c ^. Abstract.constructorPragmas
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
  Abstract.ExpressionLet {} -> unsupported "let in a constructor type"
  Abstract.ExpressionCase {} -> unsupported "case in a constructor type"
  where
    viewFunctionType :: Abstract.Function -> Sem r (NonEmpty Expression, Expression)
    viewFunctionType (Abstract.Function p r) = do
      (args, ret) <- viewConstructorType r
      p' <- goFunctionParameter p
      return $ case p' ^. paramName of
        Just {} -> unsupported "named argument in constructor type"
        Nothing -> (p' ^. paramType :| args, ret)
