module MiniJuvix.Translation.ScopedToAbstract
  ( module MiniJuvix.Translation.ScopedToAbstract,
    module MiniJuvix.Syntax.Abstract.AbstractResult,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Abstract.AbstractResult
import MiniJuvix.Syntax.Abstract.InfoTableBuilder
import MiniJuvix.Syntax.Abstract.Language (FunctionDef (_funDefTypeSig))
import MiniJuvix.Syntax.Abstract.Language qualified as Abstract
import MiniJuvix.Syntax.Concrete.Language qualified as Concrete
import MiniJuvix.Syntax.Concrete.Scoped.Error
import MiniJuvix.Syntax.Concrete.Scoped.Name qualified as S
import MiniJuvix.Syntax.Concrete.Scoped.Scoper qualified as Scoper

unsupported :: Text -> a
unsupported msg = error $ msg <> "Scoped to Abstract: not yet supported"

entryAbstract :: Member (Error ScoperError) r => Scoper.ScoperResult -> Sem r AbstractResult
entryAbstract _resultScoper = do
  (_resultTable, _resultModules) <- runInfoTableBuilder (mapM goTopModule ms)
  return AbstractResult {..}
  where
    ms = _resultScoper ^. Scoper.resultModules

goTopModule ::
  Members '[InfoTableBuilder, Error ScoperError] r =>
  Module 'Scoped 'ModuleTop ->
  Sem r Abstract.TopModule
goTopModule = goModule

goLocalModule ::
  Members '[InfoTableBuilder, Error ScoperError] r =>
  Module 'Scoped 'ModuleLocal ->
  Sem r Abstract.LocalModule
goLocalModule = goModule

goModule ::
  (Members '[InfoTableBuilder, Error ScoperError] r, ModulePathType 'Scoped t ~ S.Name' c) =>
  Module 'Scoped t ->
  Sem r (Abstract.Module c)
goModule (Module n par b) = case par of
  [] -> Abstract.Module n <$> goModuleBody b
  _ -> unsupported "Module parameters"

goModuleBody ::
  forall r.
  Members '[InfoTableBuilder, Error ScoperError] r =>
  [Statement 'Scoped] ->
  Sem r Abstract.ModuleBody
goModuleBody ss' = do
  otherThanFunctions <- mapMaybeM goStatement ss
  functions <- map (fmap Abstract.StatementFunction) <$> compiledFunctions
  let _moduleStatements =
        map
          (^. indexedThing)
          ( sortOn
              (^. indexedIx)
              (otherThanFunctions <> functions)
          )
  return Abstract.ModuleBody {..}
  where
    ss :: [Indexed (Statement 'Scoped)]
    ss = zipWith Indexed [0 ..] ss'

    compiledFunctions :: Sem r [Indexed Abstract.FunctionDef]
    compiledFunctions =
      sequence $
        [ Indexed i <$> funDef
          | Indexed i sig <- sigs,
            let name = sig ^. sigName,
            let funDef = goFunctionDef sig (getClauses name)
        ]
      where
        getClauses :: S.Symbol -> NonEmpty (FunctionClause 'Scoped)
        getClauses name =
          fromMaybe impossible $
            nonEmpty
              [c | StatementFunctionClause c <- ss', name == c ^. clauseOwnerFunction]
        sigs :: [Indexed (TypeSignature 'Scoped)]
        sigs = [Indexed i t | (Indexed i (StatementTypeSignature t)) <- ss]

goStatement ::
  forall r.
  Members '[InfoTableBuilder, Error ScoperError] r =>
  Indexed (Statement 'Scoped) ->
  Sem r (Maybe (Indexed Abstract.Statement))
goStatement (Indexed idx s) =
  fmap (Indexed idx) <$> case s of
    StatementAxiom d -> Just . Abstract.StatementAxiom <$> goAxiom d
    StatementImport (Import t) -> Just . Abstract.StatementImport <$> goModule t
    StatementOperator {} -> return Nothing
    StatementOpenModule o -> goOpenModule o
    StatementEval {} -> unsupported "eval statements"
    StatementPrint {} -> unsupported "print statements"
    StatementInductive i -> Just . Abstract.StatementInductive <$> goInductive i
    StatementForeign f -> return (Just (Abstract.StatementForeign f))
    StatementModule f -> Just . Abstract.StatementLocalModule <$> goLocalModule f
    StatementTypeSignature {} -> return Nothing
    StatementFunctionClause {} -> return Nothing
    StatementCompile {} -> return Nothing

goOpenModule ::
  forall r.
  Members '[InfoTableBuilder, Error ScoperError] r =>
  OpenModule 'Scoped ->
  Sem r (Maybe Abstract.Statement)
goOpenModule o
  | o ^. openModuleImport =
      case o ^. openModuleName of
        ModuleRef' (SModuleTop :&: m) ->
          Just . Abstract.StatementImport
            <$> goModule (m ^. moduleRefModule)
        _ -> impossible
  | otherwise = return Nothing

goFunctionDef ::
  forall r.
  Members '[InfoTableBuilder, Error ScoperError] r =>
  TypeSignature 'Scoped ->
  NonEmpty (FunctionClause 'Scoped) ->
  Sem r Abstract.FunctionDef
goFunctionDef TypeSignature {..} clauses = do
  let _funDefName = _sigName
      _funDefTerminating = _sigTerminating
  _funDefClauses <- mapM goFunctionClause clauses
  _funDefTypeSig <- goExpression _sigType
  registerFunction' Abstract.FunctionDef {..}

goFunctionClause ::
  Member (Error ScoperError) r =>
  FunctionClause 'Scoped ->
  Sem r Abstract.FunctionClause
goFunctionClause FunctionClause {..} = do
  _clausePatterns' <- mapM goPattern _clausePatterns
  _clauseBody' <- goExpression _clauseBody
  goWhereBlock _clauseWhere
  return
    Abstract.FunctionClause
      { _clausePatterns = _clausePatterns',
        _clauseBody = _clauseBody'
      }

goWhereBlock ::
  Maybe (WhereBlock 'Scoped) ->
  Sem r ()
goWhereBlock w = case w of
  Just _ -> unsupported "where block"
  Nothing -> return ()

goInductiveParameter ::
  Member (Error ScoperError) r =>
  InductiveParameter 'Scoped ->
  Sem r Abstract.FunctionParameter
goInductiveParameter InductiveParameter {..} = do
  paramType' <- goExpression _inductiveParameterType
  return
    Abstract.FunctionParameter
      { _paramType = paramType',
        _paramName = Just _inductiveParameterName,
        _paramImplicit = Explicit,
        _paramUsage = UsageOmega
      }

goInductive ::
  Members '[InfoTableBuilder, Error ScoperError] r =>
  InductiveDef 'Scoped ->
  Sem r Abstract.InductiveDef
goInductive InductiveDef {..} = do
  _inductiveParameters' <- mapM goInductiveParameter _inductiveParameters
  _inductiveType' <- mapM goExpression _inductiveType
  _inductiveConstructors' <- mapM goConstructorDef _inductiveConstructors
  inductiveInfo <-
    registerInductive
      Abstract.InductiveDef
        { _inductiveParameters = _inductiveParameters',
          _inductiveName = _inductiveName,
          _inductiveType = _inductiveType',
          _inductiveConstructors = _inductiveConstructors'
        }

  forM_ _inductiveConstructors' (registerConstructor inductiveInfo)
  return (inductiveInfo ^. inductiveInfoDef)

goConstructorDef ::
  Member (Error ScoperError) r =>
  InductiveConstructorDef 'Scoped ->
  Sem r Abstract.InductiveConstructorDef
goConstructorDef (InductiveConstructorDef c ty) =
  Abstract.InductiveConstructorDef c <$> goExpression ty

goExpression ::
  forall r.
  Member (Error ScoperError) r =>
  Expression ->
  Sem r Abstract.Expression
goExpression = \case
  ExpressionIdentifier nt -> return (goIden nt)
  ExpressionParensIdentifier nt -> return (goIden nt)
  ExpressionApplication a -> Abstract.ExpressionApplication <$> goApplication a
  ExpressionInfixApplication ia -> Abstract.ExpressionApplication <$> goInfix ia
  ExpressionPostfixApplication pa -> Abstract.ExpressionApplication <$> goPostfix pa
  ExpressionLiteral l -> return (Abstract.ExpressionLiteral l)
  ExpressionLambda {} -> unsupported "Lambda"
  ExpressionBraces b -> throw (ErrAppLeftImplicit (AppLeftImplicit b))
  ExpressionMatch {} -> unsupported "Match"
  ExpressionLetBlock {} -> unsupported "Let Block"
  ExpressionUniverse uni -> return (Abstract.ExpressionUniverse (goUniverse uni))
  ExpressionFunction func -> Abstract.ExpressionFunction <$> goFunction func
  ExpressionHole h -> return (Abstract.ExpressionHole h)
  where
    goIden :: Concrete.ScopedIden -> Abstract.Expression
    goIden x = Abstract.ExpressionIden $ case x of
      ScopedAxiom a -> Abstract.IdenAxiom (Abstract.AxiomRef (a ^. Concrete.axiomRefName))
      ScopedInductive i -> Abstract.IdenInductive (Abstract.InductiveRef (i ^. Concrete.inductiveRefName))
      ScopedVar v -> Abstract.IdenVar v
      ScopedFunction fun -> Abstract.IdenFunction (Abstract.FunctionRef (fun ^. Concrete.functionRefName))
      ScopedConstructor c -> Abstract.IdenConstructor (Abstract.ConstructorRef (c ^. Concrete.constructorRefName))

    goApplication :: Application -> Sem r Abstract.Application
    goApplication (Application l arg) = do
      l' <- goExpression l
      r' <- goExpression r
      return (Abstract.Application l' r' i)
      where
        (r, i) = case arg of
          ExpressionBraces b -> (b ^. withLocParam, Implicit)
          _ -> (arg, Explicit)

    goPostfix :: PostfixApplication -> Sem r Abstract.Application
    goPostfix (PostfixApplication l op) = do
      l' <- goExpression l
      let op' = goIden op
      return (Abstract.Application op' l' Explicit)

    goInfix :: InfixApplication -> Sem r Abstract.Application
    goInfix (InfixApplication l op r) = do
      l' <- goExpression l
      let op' = goIden op
          l'' = Abstract.ExpressionApplication (Abstract.Application op' l' Explicit)
      r' <- goExpression r
      return (Abstract.Application l'' r' Explicit)

goUniverse :: Universe -> Universe
goUniverse = id

goFunction :: Member (Error ScoperError) r => Function 'Scoped -> Sem r Abstract.Function
goFunction (Function l r) = do
  _funParameter <- goFunctionParameter l
  _funReturn <- goExpression r
  return Abstract.Function {..}

defaultUsage :: Usage
defaultUsage = UsageOmega

goUsage :: Maybe Usage -> Usage
goUsage = fromMaybe defaultUsage

goFunctionParameter ::
  Member (Error ScoperError) r =>
  FunctionParameter 'Scoped ->
  Sem r Abstract.FunctionParameter
goFunctionParameter (FunctionParameter {..}) = do
  _paramType' <- goExpression _paramType
  return
    Abstract.FunctionParameter
      { Abstract._paramUsage = goUsage _paramUsage,
        Abstract._paramType = _paramType',
        Abstract._paramImplicit = _paramImplicit,
        Abstract._paramName = _paramName
      }

goPatternApplication ::
  PatternApp ->
  Sem r Abstract.ConstructorApp
goPatternApplication a = uncurry Abstract.ConstructorApp <$> viewApp (PatternApplication a)

goPatternConstructor ::
  ConstructorRef ->
  Sem r Abstract.ConstructorApp
goPatternConstructor a = uncurry Abstract.ConstructorApp <$> viewApp (PatternConstructor a)

goInfixPatternApplication ::
  PatternInfixApp ->
  Sem r Abstract.ConstructorApp
goInfixPatternApplication a = uncurry Abstract.ConstructorApp <$> viewApp (PatternInfixApplication a)

goPostfixPatternApplication ::
  PatternPostfixApp ->
  Sem r Abstract.ConstructorApp
goPostfixPatternApplication a = uncurry Abstract.ConstructorApp <$> viewApp (PatternPostfixApplication a)

viewApp :: forall r. Pattern -> Sem r (Abstract.ConstructorRef, [Abstract.Pattern])
viewApp = \case
  PatternConstructor c -> return (goConstructorRef c, [])
  PatternApplication (PatternApp l r) -> do
    r' <- goPattern r
    second (`snoc` r') <$> viewApp l
  PatternInfixApplication (PatternInfixApp l c r) -> do
    l' <- goPattern l
    r' <- goPattern r
    return (goConstructorRef c, [l', r'])
  PatternPostfixApplication (PatternPostfixApp l c) -> do
    l' <- goPattern l
    return (goConstructorRef c, [l'])
  PatternVariable {} -> err
  PatternWildcard {} -> err
  PatternBraces {} -> err
  PatternEmpty {} -> err
  where
    err :: a
    err = error "constructor expected on the left of a pattern application"

goConstructorRef :: ConstructorRef -> Abstract.ConstructorRef
goConstructorRef (ConstructorRef' n) = Abstract.ConstructorRef n

goPattern :: Pattern -> Sem r Abstract.Pattern
goPattern p = case p of
  PatternVariable a -> return $ Abstract.PatternVariable a
  PatternConstructor c -> Abstract.PatternConstructorApp <$> goPatternConstructor c
  PatternApplication a -> Abstract.PatternConstructorApp <$> goPatternApplication a
  PatternInfixApplication a -> Abstract.PatternConstructorApp <$> goInfixPatternApplication a
  PatternPostfixApplication a -> Abstract.PatternConstructorApp <$> goPostfixPatternApplication a
  PatternWildcard i -> return (Abstract.PatternWildcard i)
  PatternEmpty -> return Abstract.PatternEmpty
  PatternBraces b -> Abstract.PatternBraces <$> goPattern b

goAxiom :: Members '[InfoTableBuilder, Error ScoperError] r => AxiomDef 'Scoped -> Sem r Abstract.AxiomDef
goAxiom AxiomDef {..} = do
  _axiomType' <- goExpression _axiomType
  registerAxiom' Abstract.AxiomDef {_axiomType = _axiomType', ..}
