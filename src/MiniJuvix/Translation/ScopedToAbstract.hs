module MiniJuvix.Translation.ScopedToAbstract
  ( module MiniJuvix.Translation.ScopedToAbstract,
    module MiniJuvix.Syntax.Abstract.AbstractResult,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Abstract.AbstractResult
import MiniJuvix.Syntax.Abstract.InfoTableBuilder
import MiniJuvix.Syntax.Abstract.Language (FunctionDef (_funDefTypeSig))
import MiniJuvix.Syntax.Abstract.Language qualified as A
import MiniJuvix.Syntax.Concrete.Language
import MiniJuvix.Syntax.Concrete.Language qualified as C
import MiniJuvix.Syntax.Concrete.Scoped.Name qualified as S
import MiniJuvix.Syntax.Concrete.Scoped.Scoper qualified as Scoper

type Err = Text

unsupported :: Members '[Error Err] r => Err -> Sem r a
unsupported msg = throw $ msg <> "Scoped to Abstract: not yet supported"

entryAbstract :: Member (Error Err) r => Scoper.ScoperResult -> Sem r AbstractResult
entryAbstract _resultScoper = do
  (_resultTable, _resultModules) <- runInfoTableBuilder (mapM goTopModule ms)
  return
    AbstractResult
      { ..
      }
  where
    ms = _resultScoper ^. Scoper.resultModules

-- translateModule :: Module 'Scoped 'ModuleTop -> Either Err (InfoTable, A.TopModule)
-- translateModule = run . runError . runInfoTableBuilder . goTopModule

goTopModule :: Members '[Error Err, InfoTableBuilder] r => Module 'Scoped 'ModuleTop -> Sem r A.TopModule
goTopModule = goModule

goLocalModule :: Members '[Error Err, InfoTableBuilder] r => Module 'Scoped 'ModuleLocal -> Sem r A.LocalModule
goLocalModule = goModule

goModule :: (Members '[Error Err, InfoTableBuilder] r, ModulePathType 'Scoped t ~ S.Name' c) => Module 'Scoped t -> Sem r (A.Module c)
goModule (Module n par b) = case par of
  [] -> A.Module n <$> goModuleBody b
  _ -> unsupported "Module parameters"

goModuleBody ::
  forall r.
  Members '[Error Err, InfoTableBuilder] r =>
  [Statement 'Scoped] ->
  Sem r A.ModuleBody
goModuleBody ss' = do
  otherThanFunctions <- mapMaybeM goStatement ss
  functions <- map (fmap A.StatementFunction) <$> compiledFunctions
  let _moduleStatements =
        map
          (^. indexedThing)
          ( sortOn
              (^. indexedIx)
              (otherThanFunctions <> functions)
          )
  return A.ModuleBody {..}
  where
    ss :: [Indexed (Statement 'Scoped)]
    ss = zipWith Indexed [0 ..] ss'
    compiledFunctions :: Sem r [Indexed A.FunctionDef]
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
  Members '[Error Err, InfoTableBuilder] r =>
  Indexed (Statement 'Scoped) ->
  Sem r (Maybe (Indexed A.Statement))
goStatement (Indexed idx s) =
  fmap (Indexed idx) <$> case s of
    StatementAxiom d -> Just . A.StatementAxiom <$> goAxiom d
    StatementImport (Import t) -> Just . A.StatementImport <$> goModule t
    StatementOperator {} -> return Nothing
    StatementOpenModule {} -> return Nothing
    StatementEval {} -> unsupported "eval statements"
    StatementPrint {} -> unsupported "print statements"
    StatementInductive i -> Just . A.StatementInductive <$> goInductive i
    StatementForeign f -> return (Just (A.StatementForeign f))
    StatementModule f -> Just . A.StatementLocalModule <$> goLocalModule f
    StatementTypeSignature {} -> return Nothing
    StatementFunctionClause {} -> return Nothing

goFunctionDef :: forall r. Members '[Error Err, InfoTableBuilder] r => TypeSignature 'Scoped -> NonEmpty (FunctionClause 'Scoped) -> Sem r A.FunctionDef
goFunctionDef sig clauses = do
  let _funDefName = sig ^. sigName
  _funDefClauses <- mapM goFunctionClause clauses
  _funDefTypeSig <- goExpression (sig ^. sigType)
  registerFunction' A.FunctionDef {..}

goFunctionClause :: forall r. Members '[Error Err] r => FunctionClause 'Scoped -> Sem r A.FunctionClause
goFunctionClause FunctionClause {..} = do
  _clausePatterns' <- mapM goPattern _clausePatterns
  _clauseBody' <- goExpression _clauseBody
  goWhereBlock _clauseWhere
  return
    A.FunctionClause
      { _clausePatterns = _clausePatterns',
        _clauseBody = _clauseBody'
      }

goWhereBlock :: forall r. Members '[Error Err] r => Maybe (WhereBlock 'Scoped) -> Sem r ()
goWhereBlock w = case w of
  Just _ -> unsupported "where block"
  Nothing -> return ()

goInductiveParameter :: Members '[Error Err] r => InductiveParameter 'Scoped -> Sem r A.FunctionParameter
goInductiveParameter InductiveParameter {..} = do
  paramType' <- goExpression _inductiveParameterType
  return
    A.FunctionParameter
      { _paramType = paramType',
        _paramName = Just _inductiveParameterName,
        _paramUsage = UsageOmega
      }

goInductive :: Members '[Error Err, InfoTableBuilder] r => InductiveDef 'Scoped -> Sem r A.InductiveDef
goInductive InductiveDef {..} = do
  _inductiveParameters' <- mapM goInductiveParameter _inductiveParameters
  _inductiveType' <- sequence $ goExpression <$> _inductiveType
  _inductiveConstructors' <- mapM goConstructorDef _inductiveConstructors
  inductiveInfo <-
    registerInductive
      A.InductiveDef
        { _inductiveParameters = _inductiveParameters',
          _inductiveName = _inductiveName,
          _inductiveType = _inductiveType',
          _inductiveConstructors = _inductiveConstructors'
        }

  forM_ _inductiveConstructors' (registerConstructor inductiveInfo)

  return (inductiveInfo ^. inductiveInfoDef)

goConstructorDef :: Members '[Error Err] r => InductiveConstructorDef 'Scoped -> Sem r A.InductiveConstructorDef
goConstructorDef (InductiveConstructorDef c ty) = A.InductiveConstructorDef c <$> goExpression ty

goExpression :: forall r. Members '[Error Err] r => Expression -> Sem r A.Expression
goExpression e = case e of
  ExpressionIdentifier nt -> return (goIden nt)
  ExpressionParensIdentifier nt -> return (goIden nt)
  ExpressionApplication a -> A.ExpressionApplication <$> goApplication a
  ExpressionInfixApplication ia -> A.ExpressionApplication <$> goInfix ia
  ExpressionPostfixApplication pa -> A.ExpressionApplication <$> goPostfix pa
  ExpressionLiteral l -> return $ A.ExpressionLiteral l
  ExpressionLambda {} -> unsupported "Lambda"
  ExpressionMatch {} -> unsupported "Match"
  ExpressionLetBlock {} -> unsupported "Let Block"
  ExpressionUniverse uni -> return $ A.ExpressionUniverse (goUniverse uni)
  ExpressionFunction func -> A.ExpressionFunction <$> goFunction func
  where
    goIden :: C.ScopedIden -> A.Expression
    goIden x = A.ExpressionIden $ case x of
      ScopedAxiom a -> A.IdenAxiom (A.AxiomRef (a ^. C.axiomRefName))
      ScopedInductive i -> A.IdenInductive (A.InductiveRef (i ^. C.inductiveRefName))
      ScopedVar v -> A.IdenVar v
      ScopedFunction fun -> A.IdenFunction (A.FunctionRef (fun ^. C.functionRefName))
      ScopedConstructor c -> A.IdenConstructor (A.ConstructorRef (c ^. C.constructorRefName))

    goApplication :: Application -> Sem r A.Application
    goApplication (Application l r) = do
      l' <- goExpression l
      r' <- goExpression r
      return (A.Application l' r')

    goPostfix :: PostfixApplication -> Sem r A.Application
    goPostfix (PostfixApplication l op) = do
      l' <- goExpression l
      let op' = goIden op
      return (A.Application op' l')

    goInfix :: InfixApplication -> Sem r A.Application
    goInfix (InfixApplication l op r) = do
      l' <- goExpression l
      let op' = goIden op
      r' <- goExpression r
      return $ A.Application (A.ExpressionApplication (A.Application op' l')) r'

goUniverse :: Universe -> Universe
goUniverse = id

goFunction :: Members '[Error Err] r => Function 'Scoped -> Sem r A.Function
goFunction (Function l r) = do
  _funParameter <- goFunctionParameter l
  _funReturn <- goExpression r
  return A.Function {..}

defaultUsage :: Usage
defaultUsage = UsageOmega

goUsage :: Maybe Usage -> Usage
goUsage = fromMaybe defaultUsage

goFunctionParameter :: Members '[Error Err] r => FunctionParameter 'Scoped -> Sem r A.FunctionParameter
goFunctionParameter (FunctionParameter _paramName u ty) = do
  _paramType <- goExpression ty
  let _paramUsage = goUsage u
  return A.FunctionParameter {..}

goPatternApplication :: forall r. Members '[Error Err] r => PatternApp -> Sem r A.ConstructorApp
goPatternApplication a = uncurry A.ConstructorApp <$> viewApp (PatternApplication a)

goPatternConstructor :: forall r. Members '[Error Err] r => ConstructorRef -> Sem r A.ConstructorApp
goPatternConstructor a = uncurry A.ConstructorApp <$> viewApp (PatternConstructor a)

goInfixPatternApplication :: forall r. Members '[Error Err] r => PatternInfixApp -> Sem r A.ConstructorApp
goInfixPatternApplication a = uncurry A.ConstructorApp <$> viewApp (PatternInfixApplication a)

goPostfixPatternApplication :: forall r. Members '[Error Err] r => PatternPostfixApp -> Sem r A.ConstructorApp
goPostfixPatternApplication a = uncurry A.ConstructorApp <$> viewApp (PatternPostfixApplication a)

viewApp :: forall r. Members '[Error Err] r => Pattern -> Sem r (A.ConstructorRef, [A.Pattern])
viewApp p = case p of
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
  PatternEmpty {} -> err
  where
    err :: Sem r a
    err = throw ("constructor expected on the left of a pattern application" :: Err)

goConstructorRef :: ConstructorRef -> A.ConstructorRef
goConstructorRef (ConstructorRef' n) = A.ConstructorRef n

goPattern :: forall r. Members '[Error Err] r => Pattern -> Sem r A.Pattern
goPattern p = case p of
  PatternVariable a -> return $ A.PatternVariable a
  PatternConstructor c -> A.PatternConstructorApp <$> goPatternConstructor c
  PatternApplication a -> A.PatternConstructorApp <$> goPatternApplication a
  PatternInfixApplication a -> A.PatternConstructorApp <$> goInfixPatternApplication a
  PatternPostfixApplication a -> A.PatternConstructorApp <$> goPostfixPatternApplication a
  PatternWildcard -> return A.PatternWildcard
  PatternEmpty -> return A.PatternEmpty

goAxiom :: Members '[Error Err, InfoTableBuilder] r => AxiomDef 'Scoped -> Sem r A.AxiomDef
goAxiom AxiomDef {..} = do
  _axiomType' <- goExpression _axiomType
  registerAxiom' A.AxiomDef {_axiomType = _axiomType', ..}
