module MiniJuvix.Translation.ScopedToAbstract
  ( module MiniJuvix.Translation.ScopedToAbstract,
    module MiniJuvix.Syntax.Abstract.AbstractResult,
  )
where

import Data.HashMap.Strict qualified as HashMap
import MiniJuvix.Builtins
import MiniJuvix.Internal.NameIdGen
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Abstract.AbstractResult
import MiniJuvix.Syntax.Abstract.InfoTableBuilder
import MiniJuvix.Syntax.Abstract.Language qualified as Abstract
import MiniJuvix.Syntax.Concrete.Language qualified as Concrete
import MiniJuvix.Syntax.Concrete.Scoped.Error
import MiniJuvix.Syntax.Concrete.Scoped.Name qualified as S
import MiniJuvix.Syntax.Concrete.Scoped.Name.NameKind
import MiniJuvix.Syntax.Concrete.Scoped.Scoper qualified as Scoper

newtype ModulesCache = ModulesCache
  {_cachedModules :: HashMap S.NameId Abstract.TopModule}

makeLenses ''ModulesCache

unsupported :: Text -> a
unsupported msg = error $ msg <> "Scoped to Abstract: not yet supported"

entryAbstract :: Members '[Error ScoperError, Builtins, NameIdGen] r => Scoper.ScoperResult -> Sem r AbstractResult
entryAbstract _resultScoper = do
  (_resultTable, _resultModules) <- runInfoTableBuilder (evalState (ModulesCache mempty) (mapM goTopModule ms))
  return AbstractResult {..}
  where
    ms = _resultScoper ^. Scoper.resultModules

goTopModule ::
  Members '[InfoTableBuilder, Error ScoperError, Builtins, NameIdGen, State ModulesCache] r =>
  Module 'Scoped 'ModuleTop ->
  Sem r Abstract.TopModule
goTopModule = goModule

goLocalModule ::
  Members '[InfoTableBuilder, Error ScoperError, Builtins, NameIdGen, State ModulesCache] r =>
  Module 'Scoped 'ModuleLocal ->
  Sem r Abstract.LocalModule
goLocalModule = goModule

goModule ::
  forall r t.
  (Members '[InfoTableBuilder, Error ScoperError, Builtins, NameIdGen, State ModulesCache] r, SingI t) =>
  Module 'Scoped t ->
  Sem r Abstract.Module
goModule m = case sing :: SModuleIsTop t of
  SModuleTop -> do
    cache <- gets (^. cachedModules)
    let moduleNameId :: S.NameId
        moduleNameId = m ^. Concrete.modulePath . S.nameId
    let processModule :: Sem r Abstract.Module
        processModule = do
          am <- goModule' m
          modify (over cachedModules (HashMap.insert moduleNameId am))
          return am
    maybe processModule return (cache ^. at moduleNameId)
  SModuleLocal -> goModule' m
  where
    goModule' :: Module 'Scoped t -> Sem r Abstract.Module
    goModule' (Module n par b) = case par of
      [] -> Abstract.Module modName <$> goModuleBody b
      _ -> unsupported "Module parameters"
      where
        modName :: Abstract.Name
        modName = case sing :: SModuleIsTop t of
          SModuleTop -> goSymbol (S.topModulePathName n)
          SModuleLocal -> goSymbol n

goName :: S.Name -> Abstract.Name
goName = goSymbol . S.nameUnqualify

goSymbol :: S.Symbol -> Abstract.Name
goSymbol s =
  Abstract.Name
    { _nameText = S.symbolText s,
      _nameId = s ^. S.nameId,
      _nameKind = getNameKind s,
      _nameLoc = s ^. S.nameConcrete . symbolLoc
    }

goModuleBody ::
  forall r.
  Members '[InfoTableBuilder, Error ScoperError, Builtins, NameIdGen, State ModulesCache] r =>
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
  Members '[InfoTableBuilder, Error ScoperError, Builtins, NameIdGen, State ModulesCache] r =>
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
  Members '[InfoTableBuilder, Error ScoperError, Builtins, NameIdGen, State ModulesCache] r =>
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
  Members '[InfoTableBuilder, Error ScoperError, Builtins, NameIdGen] r =>
  TypeSignature 'Scoped ->
  NonEmpty (FunctionClause 'Scoped) ->
  Sem r Abstract.FunctionDef
goFunctionDef TypeSignature {..} clauses = do
  let _funDefName = goSymbol _sigName
      _funDefTerminating = _sigTerminating
      _funDefBuiltin = _sigBuiltin
  _funDefClauses <- mapM goFunctionClause clauses
  _funDefTypeSig <- goExpression _sigType
  let fun = Abstract.FunctionDef {..}
  whenJust _sigBuiltin (registerBuiltinFunction fun)
  registerFunction' fun

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
      { _clauseName = goSymbol _clauseOwnerFunction,
        _clausePatterns = _clausePatterns',
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
        _paramName = Just (goSymbol _inductiveParameterName),
        _paramImplicit = Explicit,
        _paramUsage = UsageOmega
      }

registerBuiltinInductive ::
  Members '[InfoTableBuilder, Error ScoperError, Builtins] r =>
  Abstract.InductiveDef ->
  BuiltinInductive ->
  Sem r ()
registerBuiltinInductive d = \case
  BuiltinNatural -> registerNaturalDef d

registerBuiltinFunction ::
  Members '[InfoTableBuilder, Error ScoperError, Builtins, NameIdGen] r =>
  Abstract.FunctionDef ->
  BuiltinFunction ->
  Sem r ()
registerBuiltinFunction d = \case
  BuiltinNaturalPlus -> registerNaturalPlus d

registerBuiltinAxiom ::
  Members '[InfoTableBuilder, Error ScoperError, Builtins] r =>
  Abstract.AxiomDef ->
  BuiltinAxiom ->
  Sem r ()
registerBuiltinAxiom d = \case
  BuiltinIO -> registerIO d
  BuiltinIOSequence -> registerIOSequence d
  BuiltinNaturalPrint -> registerNaturalPrint d

goInductive ::
  Members '[InfoTableBuilder, Builtins, Error ScoperError] r =>
  InductiveDef 'Scoped ->
  Sem r Abstract.InductiveDef
goInductive InductiveDef {..} = do
  _inductiveParameters' <- mapM goInductiveParameter _inductiveParameters
  _inductiveType' <- mapM goExpression _inductiveType
  _inductiveConstructors' <- mapM goConstructorDef _inductiveConstructors
  let loc = getLoc _inductiveName
  let indDef =
        Abstract.InductiveDef
          { _inductiveParameters = _inductiveParameters',
            _inductiveBuiltin = _inductiveBuiltin,
            _inductiveName = goSymbol _inductiveName,
            _inductiveType = fromMaybe (Abstract.ExpressionUniverse (smallUniverse loc)) _inductiveType',
            _inductiveConstructors = _inductiveConstructors'
          }
  whenJust _inductiveBuiltin (registerBuiltinInductive indDef)
  inductiveInfo <- registerInductive indDef
  forM_ _inductiveConstructors' (registerConstructor inductiveInfo)
  return (inductiveInfo ^. inductiveInfoDef)

goConstructorDef ::
  Member (Error ScoperError) r =>
  InductiveConstructorDef 'Scoped ->
  Sem r Abstract.InductiveConstructorDef
goConstructorDef (InductiveConstructorDef c ty) =
  Abstract.InductiveConstructorDef (goSymbol c) <$> goExpression ty

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
      ScopedAxiom a -> Abstract.IdenAxiom (Abstract.AxiomRef (goName (a ^. Concrete.axiomRefName)))
      ScopedInductive i -> Abstract.IdenInductive (Abstract.InductiveRef (goName (i ^. Concrete.inductiveRefName)))
      ScopedVar v -> Abstract.IdenVar (goSymbol v)
      ScopedFunction fun -> Abstract.IdenFunction (Abstract.FunctionRef (goName (fun ^. Concrete.functionRefName)))
      ScopedConstructor c -> Abstract.IdenConstructor (Abstract.ConstructorRef (goName (c ^. Concrete.constructorRefName)))

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
        Abstract._paramName = goSymbol <$> _paramName
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
goConstructorRef (ConstructorRef' n) = Abstract.ConstructorRef (goName n)

goPattern :: Pattern -> Sem r Abstract.Pattern
goPattern p = case p of
  PatternVariable a -> return $ Abstract.PatternVariable (goSymbol a)
  PatternConstructor c -> Abstract.PatternConstructorApp <$> goPatternConstructor c
  PatternApplication a -> Abstract.PatternConstructorApp <$> goPatternApplication a
  PatternInfixApplication a -> Abstract.PatternConstructorApp <$> goInfixPatternApplication a
  PatternPostfixApplication a -> Abstract.PatternConstructorApp <$> goPostfixPatternApplication a
  PatternWildcard i -> return (Abstract.PatternWildcard i)
  PatternEmpty -> return Abstract.PatternEmpty
  PatternBraces b -> Abstract.PatternBraces <$> goPattern b

goAxiom :: Members '[InfoTableBuilder, Error ScoperError, Builtins] r => AxiomDef 'Scoped -> Sem r Abstract.AxiomDef
goAxiom a = do
  _axiomType' <- goExpression (a ^. axiomType)
  let axiom =
        Abstract.AxiomDef
          { _axiomType = _axiomType',
            _axiomBuiltin = a ^. axiomBuiltin,
            _axiomName = goSymbol (a ^. axiomName)
          }
  whenJust (a ^. axiomBuiltin) (registerBuiltinAxiom axiom)
  registerAxiom' axiom
