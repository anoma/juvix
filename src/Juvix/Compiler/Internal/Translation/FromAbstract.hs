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
import Juvix.Compiler.Abstract.Language qualified as Abstract
import Juvix.Compiler.Abstract.Translation.FromConcrete.Data.Context qualified as Abstract
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context qualified as Scoped
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Pretty.Base
import Juvix.Compiler.Internal.Translation.FromAbstract.Data.Context
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker
import Juvix.Compiler.Pipeline.EntryPoint qualified as E
import Juvix.Prelude

fromAbstract ::
  Members '[Error JuvixError, NameIdGen] r =>
  Abstract.AbstractResult ->
  Sem r InternalResult
fromAbstract abstractResults = do
  let abstractModules = abstractResults ^. Abstract.resultModules
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
    exportsTbl :: HashSet NameId = abstractResults ^. Abstract.resultScoper . Scoped.resultExports
    noTerminationOption =
      abstractResults
        ^. Abstract.abstractResultEntryPoint
          . E.entryPointNoTermination
    depInfo = buildDependencyInfo (abstractResults ^. Abstract.resultModules) exportsTbl

fromAbstractExpression :: Members '[NameIdGen] r => Abstract.Expression -> Sem r Expression
fromAbstractExpression e = runReader depInfo (goExpression e) >>= checkTypesSupported
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
    checkTypesSupported $
      Module
        { _moduleName = m ^. Abstract.moduleName,
          _moduleExamples = examples',
          _moduleBody = _moduleBody',
          _modulePragmas = m ^. Abstract.modulePragmas
        }

checkTypesSupported :: Data a => a -> Sem r a
checkTypesSupported a = do
  mapM_ checkType (allTypeSignatures a)
  return a
  where
    checkTypeIden :: Iden -> Sem r ()
    checkTypeIden = \case
      IdenConstructor {} -> unsupported "constructors in types"
      IdenFunction {} -> return ()
      IdenAxiom {} -> return ()
      IdenVar {} -> return ()
      IdenInductive {} -> return ()
    checkType :: Expression -> Sem r ()
    checkType = \case
      ExpressionLiteral {} -> unsupported "literals in types"
      ExpressionLambda {} -> unsupported "lambda in types"
      ExpressionLet {} -> unsupported "let in types"
      ExpressionCase {} -> unsupported "case in types"
      ExpressionIden i -> checkTypeIden i
      ExpressionApplication {} -> return ()
      ExpressionFunction {} -> return ()
      ExpressionHole {} -> return ()
      ExpressionUniverse {} -> return ()
      ExpressionSimpleLambda {} -> return ()

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

goStatement ::
  forall r.
  Members '[Reader ExportsTable, Reader NameDependencyInfo, NameIdGen] r =>
  Abstract.Statement ->
  Sem r [Statement]
goStatement = \case
  Abstract.StatementAxiom a -> pure . StatementAxiom <$> goAxiomDef a
  Abstract.StatementMutual {} -> impossible

goModuleBody ::
  forall r.
  Members '[Reader ExportsTable, Reader NameDependencyInfo, NameIdGen] r =>
  Abstract.ModuleBody ->
  Sem r ModuleBody
goModuleBody Abstract.ModuleBody {..} = do
  stmts <- concatMapM goStatement _moduleStatements
  let imports :: [Abstract.Include] = _moduleIncludes
  imports' <- mapM goInclude imports
  return
    ModuleBody
      { _moduleStatements = stmts,
        _moduleIncludes = imports'
      }

goAxiomDef :: Members '[NameIdGen, Reader NameDependencyInfo] r => Abstract.AxiomDef -> Sem r AxiomDef
goAxiomDef a = do
  _axiomType' <- goExpression (a ^. Abstract.axiomType)
  return
    AxiomDef
      { _axiomName = a ^. Abstract.axiomName,
        _axiomBuiltin = a ^. Abstract.axiomBuiltin,
        _axiomType = _axiomType',
        _axiomPragmas = a ^. Abstract.axiomPragmas
      }

goFunctionParameter :: Members '[NameIdGen, Reader NameDependencyInfo] r => Abstract.FunctionParameter -> Sem r FunctionParameter
goFunctionParameter f = do
  _paramType <- goExpression (f ^. Abstract.paramType)
  return
    FunctionParameter
      { _paramName = f ^. Abstract.paramName,
        _paramImplicit = f ^. Abstract.paramImplicit,
        _paramType
      }

goFunction :: Members '[NameIdGen, Reader NameDependencyInfo] r => Abstract.Function -> Sem r Function
goFunction (Abstract.Function l r) = do
  l' <- goFunctionParameter l
  r' <- goExpression r
  return (Function l' r')

goFunctionDef :: Members '[NameIdGen, Reader NameDependencyInfo] r => Abstract.FunctionDef -> Sem r FunctionDef
goFunctionDef f = do
  _funDefClauses' <- mapM (goFunctionClause _funDefName') (f ^. Abstract.funDefClauses)
  _funDefType' <- goExpression (f ^. Abstract.funDefTypeSig)
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

goConstructorApp :: (Members '[NameIdGen] r) => Abstract.ConstructorApp -> Sem r ConstructorApp
goConstructorApp c = do
  _constrAppParameters' <- mapM goPatternArg (c ^. Abstract.constrAppParameters)
  return
    ConstructorApp
      { _constrAppConstructor = c ^. Abstract.constrAppConstructor,
        _constrAppParameters = _constrAppParameters',
        _constrAppType = Nothing
      }

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

goExpression :: Members '[NameIdGen, Reader NameDependencyInfo] r => Abstract.Expression -> Sem r Expression
goExpression e = case e of
  Abstract.ExpressionIden i -> return (ExpressionIden (goIden i))
  Abstract.ExpressionUniverse u -> return (ExpressionUniverse u)
  Abstract.ExpressionFunction f -> ExpressionFunction <$> goFunction f
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

-- goInductiveParameter :: Abstract.InductiveParameter -> InductiveParameter
-- goInductiveParameter f =
--   InductiveParameter
--     { _inductiveParamName = f ^. Abstract.inductiveParamName
--     }

-- goInductiveDef :: forall r. Members '[NameIdGen, Reader NameDependencyInfo] r => Abstract.InductiveDef -> Sem r InductiveDef
-- goInductiveDef i = do
--   let inductiveParameters' = map goInductiveParameter (i ^. Abstract.inductiveParameters)
--       indTypeName = i ^. Abstract.inductiveName
--   inductiveConstructors' <-
--     mapM
--       goConstructorDef
--       (i ^. Abstract.inductiveConstructors)
--   examples' <- mapM goExample (i ^. Abstract.inductiveExamples)
--   ty' <- goExpression (i ^. Abstract.inductiveType)
--   return
--     InductiveDef
--       { _inductiveName = indTypeName,
--         _inductiveParameters = inductiveParameters',
--         _inductiveBuiltin = i ^. Abstract.inductiveBuiltin,
--         _inductiveConstructors = inductiveConstructors',
--         _inductiveExamples = examples',
--         _inductiveType = ty',
--         _inductivePositive = i ^. Abstract.inductivePositive,
--         _inductivePragmas = i ^. Abstract.inductivePragmas
--       }
--   where
--     goConstructorDef :: Abstract.InductiveConstructorDef -> Sem r InductiveConstructorDef
--     goConstructorDef c = do
--       ty' <- goExpression (c ^. Abstract.constructorType)
--       examples' <- mapM goExample (c ^. Abstract.constructorExamples)
--       return
--         InductiveConstructorDef
--           { _inductiveConstructorName = c ^. Abstract.constructorName,
--             _inductiveConstructorExamples = examples',
--             _inductiveConstructorType = ty',
--             _inductiveConstructorPragmas = c ^. Abstract.constructorPragmas
--           }
