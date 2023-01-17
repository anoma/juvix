module Juvix.Compiler.Abstract.Translation.FromConcrete
  ( module Juvix.Compiler.Abstract.Translation.FromConcrete,
    module Juvix.Compiler.Abstract.Translation.FromConcrete.Data.Context,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Abstract.Data.InfoTableBuilder
import Juvix.Compiler.Abstract.Language (FunctionDef (_funDefExamples))
import Juvix.Compiler.Abstract.Language qualified as Abstract
import Juvix.Compiler.Abstract.Translation.FromConcrete.Data.Context
import Juvix.Compiler.Builtins
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language qualified as Concrete
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error
import Juvix.Data.NameKind
import Juvix.Prelude

newtype ModulesCache = ModulesCache
  {_cachedModules :: HashMap S.NameId Abstract.TopModule}

makeLenses ''ModulesCache

unsupported :: Text -> a
unsupported msg = error $ msg <> "Scoped to Abstract: not yet supported"

fromConcrete :: Members '[Error JuvixError, Builtins, NameIdGen] r => Scoper.ScoperResult -> Sem r AbstractResult
fromConcrete _resultScoper =
  mapError (JuvixError @ScoperError) $ do
    (_resultTable, _resultModules) <- runInfoTableBuilder (evalState (ModulesCache mempty) (mapM goTopModule ms))
    let _resultExports = _resultScoper ^. Scoper.resultExports
    return AbstractResult {..}
  where
    ms = _resultScoper ^. Scoper.resultModules

fromConcreteExpression :: Members '[Error JuvixError, NameIdGen] r => Scoper.Expression -> Sem r Abstract.Expression
fromConcreteExpression = mapError (JuvixError @ScoperError) . ignoreInfoTableBuilder . goExpression

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
    goModule' Module {..}
      | null _moduleParameters = do
          body' <- goModuleBody _moduleBody
          examples' <- goExamples _moduleDoc
          return
            Abstract.Module
              { _moduleName = name',
                _moduleBody = body',
                _moduleExamples = examples'
              }
      | otherwise = unsupported "Module parameters"
      where
        name' :: Abstract.Name
        name' = case sing :: SModuleIsTop t of
          SModuleTop -> goSymbol (S.topModulePathName _modulePath)
          SModuleLocal -> goSymbol _modulePath

goName :: S.Name -> Abstract.Name
goName name =
  set Abstract.namePretty prettyStr (goSymbol (S.nameUnqualify name))
  where
    prettyStr :: Text
    prettyStr = prettyText name

goSymbol :: S.Symbol -> Abstract.Name
goSymbol s =
  Abstract.Name
    { _nameText = S.symbolText s,
      _nameId = s ^. S.nameId,
      _nameKind = getNameKind s,
      _namePretty = S.symbolText s,
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
  _funDefExamples <- goExamples _sigDoc
  let fun = Abstract.FunctionDef {..}
  whenJust _sigBuiltin (registerBuiltinFunction fun)
  registerFunction' fun

goExamples ::
  forall r.
  Members '[Error ScoperError, InfoTableBuilder] r =>
  Maybe (Judoc 'Scoped) ->
  Sem r [Abstract.Example]
goExamples = mapM goExample . maybe [] judocExamples
  where
    goExample :: Example 'Scoped -> Sem r Abstract.Example
    goExample ex = do
      e' <- goExpression (ex ^. exampleExpression)
      return
        Abstract.Example
          { _exampleExpression = e',
            _exampleId = ex ^. exampleId
          }

goFunctionClause ::
  Members '[Error ScoperError, InfoTableBuilder] r =>
  FunctionClause 'Scoped ->
  Sem r Abstract.FunctionClause
goFunctionClause FunctionClause {..} = do
  _clausePatterns' <- mapM goPatternArg _clausePatterns
  _clauseBody' <- goExpression _clauseBody
  return
    Abstract.FunctionClause
      { _clauseName = goSymbol _clauseOwnerFunction,
        _clausePatterns = _clausePatterns',
        _clauseBody = _clauseBody'
      }

goInductiveParameter ::
  Members '[Error ScoperError, InfoTableBuilder] r =>
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
  BuiltinNat -> registerNatDef d
  BuiltinBool -> registerBoolDef d

registerBuiltinFunction ::
  Members '[InfoTableBuilder, Error ScoperError, Builtins, NameIdGen] r =>
  Abstract.FunctionDef ->
  BuiltinFunction ->
  Sem r ()
registerBuiltinFunction d = \case
  BuiltinNatPlus -> registerNatPlus d
  BuiltinNatSub -> registerNatSub d
  BuiltinNatMul -> registerNatMul d
  BuiltinNatUDiv -> registerNatUDiv d
  BuiltinNatDiv -> registerNatDiv d
  BuiltinNatMod -> registerNatMod d
  BuiltinNatLe -> registerNatLe d
  BuiltinNatLt -> registerNatLt d
  BuiltinNatEq -> registerNatEq d
  BuiltinBoolIf -> registerIf d

registerBuiltinAxiom ::
  Members '[InfoTableBuilder, Error ScoperError, Builtins] r =>
  Abstract.AxiomDef ->
  BuiltinAxiom ->
  Sem r ()
registerBuiltinAxiom d = \case
  BuiltinIO -> registerIO d
  BuiltinIOSequence -> registerIOSequence d
  BuiltinNatPrint -> registerNatPrint d
  BuiltinString -> registerString d
  BuiltinStringPrint -> registerStringPrint d
  BuiltinBoolPrint -> registerBoolPrint d

goInductive ::
  Members '[InfoTableBuilder, Builtins, Error ScoperError] r =>
  InductiveDef 'Scoped ->
  Sem r Abstract.InductiveDef
goInductive ty@InductiveDef {..} = do
  _inductiveParameters' <- mapM goInductiveParameter _inductiveParameters
  _inductiveType' <- mapM goExpression _inductiveType
  _inductiveConstructors' <- mapM goConstructorDef _inductiveConstructors
  _inductiveExamples' <- goExamples _inductiveDoc
  let loc = getLoc _inductiveName
      indDef =
        Abstract.InductiveDef
          { _inductiveParameters = _inductiveParameters',
            _inductiveBuiltin = _inductiveBuiltin,
            _inductiveName = goSymbol _inductiveName,
            _inductiveType = fromMaybe (Abstract.ExpressionUniverse (smallUniverse loc)) _inductiveType',
            _inductiveConstructors = toList _inductiveConstructors',
            _inductiveExamples = _inductiveExamples',
            _inductivePositive = ty ^. inductivePositive
          }
  whenJust _inductiveBuiltin (registerBuiltinInductive indDef)
  inductiveInfo <- registerInductive indDef
  forM_ _inductiveConstructors' (registerConstructor inductiveInfo)
  return (inductiveInfo ^. inductiveInfoDef)

goConstructorDef ::
  Members [Error ScoperError, InfoTableBuilder] r =>
  InductiveConstructorDef 'Scoped ->
  Sem r Abstract.InductiveConstructorDef
goConstructorDef InductiveConstructorDef {..} = do
  ty' <- goExpression _constructorType
  examples' <- goExamples _constructorDoc
  return
    Abstract.InductiveConstructorDef
      { _constructorType = ty',
        _constructorExamples = examples',
        _constructorName = goSymbol _constructorName
      }

goExpression ::
  forall r.
  Members [Error ScoperError, InfoTableBuilder] r =>
  Expression ->
  Sem r Abstract.Expression
goExpression = \case
  ExpressionIdentifier nt -> return (goIden nt)
  ExpressionParensIdentifier nt -> return (goIden nt)
  ExpressionApplication a -> Abstract.ExpressionApplication <$> goApplication a
  ExpressionInfixApplication ia -> Abstract.ExpressionApplication <$> goInfix ia
  ExpressionPostfixApplication pa -> Abstract.ExpressionApplication <$> goPostfix pa
  ExpressionLiteral l -> return (Abstract.ExpressionLiteral l)
  ExpressionLambda l -> Abstract.ExpressionLambda <$> goLambda l
  ExpressionBraces b -> throw (ErrAppLeftImplicit (AppLeftImplicit b))
  ExpressionLetBlock l -> Abstract.ExpressionLet <$> goLet l
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

    goLet :: LetBlock 'Scoped -> Sem r Abstract.Let
    goLet l = do
      _letExpression <- goExpression (l ^. letExpression)
      _letClauses <- goLetClauses (l ^. letClauses)
      return Abstract.Let {..}
      where
        goLetClauses :: NonEmpty (LetClause 'Scoped) -> Sem r (NonEmpty Abstract.LetClause)
        goLetClauses cl =
          nonEmpty' <$> sequence [Abstract.LetFunDef <$> goSig sig | LetTypeSig sig <- toList cl]
          where
            goSig :: TypeSignature 'Scoped -> Sem r Abstract.FunctionDef
            goSig sig = do
              _funDefClauses <- getClauses
              _funDefTypeSig <- goExpression (sig ^. sigType)
              let _funDefBuiltin = sig ^. sigBuiltin
                  _funDefTerminating = sig ^. sigTerminating
                  _funDefName = goSymbol (sig ^. sigName)
                  _funDefExamples :: [Abstract.Example] = []
              registerFunction' Abstract.FunctionDef {..}
              where
                getClauses :: Sem r (NonEmpty Abstract.FunctionClause)
                getClauses = do
                  cls <-
                    sequence
                      [ goFunctionClause c | LetFunClause c <- toList cl, sig ^. sigName == c ^. clauseOwnerFunction
                      ]
                  case nonEmpty cls of
                    Nothing ->
                      throw
                        ( ErrLacksFunctionClause (LacksFunctionClause sig)
                        )
                    Just r -> return r

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

goLambda :: forall r. Members '[Error ScoperError, InfoTableBuilder] r => Lambda 'Scoped -> Sem r Abstract.Lambda
goLambda (Lambda cl) = Abstract.Lambda <$> mapM goClause cl
  where
    goClause :: LambdaClause 'Scoped -> Sem r Abstract.LambdaClause
    goClause (LambdaClause ps b) = do
      ps' <- mapM goPatternArg ps
      b' <- goExpression b
      return (Abstract.LambdaClause ps' b')

goUniverse :: Universe -> Universe
goUniverse = id

goFunction :: Members '[Error ScoperError, InfoTableBuilder] r => Function 'Scoped -> Sem r Abstract.Function
goFunction (Function l r) = do
  _funParameter <- goFunctionParameter l
  _funReturn <- goExpression r
  return Abstract.Function {..}

defaultUsage :: Usage
defaultUsage = UsageOmega

goUsage :: Maybe Usage -> Usage
goUsage = fromMaybe defaultUsage

goFunctionParameter ::
  Members '[Error ScoperError, InfoTableBuilder] r =>
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
  Members '[Error ScoperError, InfoTableBuilder] r =>
  PatternApp ->
  Sem r Abstract.ConstructorApp
goPatternApplication a = uncurry Abstract.ConstructorApp <$> viewApp (PatternApplication a)

goPatternConstructor ::
  Members '[Error ScoperError, InfoTableBuilder] r =>
  ConstructorRef ->
  Sem r Abstract.ConstructorApp
goPatternConstructor a = uncurry Abstract.ConstructorApp <$> viewApp (PatternConstructor a)

goInfixPatternApplication ::
  Members '[Error ScoperError, InfoTableBuilder] r =>
  PatternInfixApp ->
  Sem r Abstract.ConstructorApp
goInfixPatternApplication a = uncurry Abstract.ConstructorApp <$> viewApp (PatternInfixApplication a)

goPostfixPatternApplication ::
  Members '[Error ScoperError, InfoTableBuilder] r =>
  PatternPostfixApp ->
  Sem r Abstract.ConstructorApp
goPostfixPatternApplication a = uncurry Abstract.ConstructorApp <$> viewApp (PatternPostfixApplication a)

viewApp :: forall r. Members '[Error ScoperError, InfoTableBuilder] r => Pattern -> Sem r (Abstract.ConstructorRef, [Abstract.PatternArg])
viewApp p = case p of
  PatternConstructor c -> return (goConstructorRef c, [])
  PatternApplication app@(PatternApp _ r) -> do
    r' <- goPatternArg r
    second (`snoc` r') <$> viewAppLeft app
  PatternInfixApplication (PatternInfixApp l c r) -> do
    l' <- goPatternArg l
    r' <- goPatternArg r
    return (goConstructorRef c, [l', r'])
  PatternPostfixApplication (PatternPostfixApp l c) -> do
    l' <- goPatternArg l
    return (goConstructorRef c, [l'])
  PatternVariable {} -> err
  PatternWildcard {} -> err
  PatternEmpty {} -> err
  where
    viewAppLeft :: PatternApp -> Sem r (Abstract.ConstructorRef, [Abstract.PatternArg])
    viewAppLeft app@(PatternApp l _)
      | Implicit <- l ^. patternArgIsImplicit = throw (ErrImplicitPatternLeftApplication (ImplicitPatternLeftApplication app))
      | otherwise = viewApp (l ^. patternArgPattern)
    err = throw (ErrConstructorExpectedLeftApplication (ConstructorExpectedLeftApplication p))

goConstructorRef :: ConstructorRef -> Abstract.ConstructorRef
goConstructorRef (ConstructorRef' n) = Abstract.ConstructorRef (goName n)

goPatternArg :: Members '[Error ScoperError, InfoTableBuilder] r => PatternArg -> Sem r Abstract.PatternArg
goPatternArg p = do
  pat' <- goPattern (p ^. patternArgPattern)
  return
    Abstract.PatternArg
      { _patternArgIsImplicit = p ^. patternArgIsImplicit,
        _patternArgName = goSymbol <$> p ^. patternArgName,
        _patternArgPattern = pat'
      }

goPattern :: Members '[Error ScoperError, InfoTableBuilder] r => Pattern -> Sem r Abstract.Pattern
goPattern p = case p of
  PatternVariable a -> return $ Abstract.PatternVariable (goSymbol a)
  PatternConstructor c -> Abstract.PatternConstructorApp <$> goPatternConstructor c
  PatternApplication a -> Abstract.PatternConstructorApp <$> goPatternApplication a
  PatternInfixApplication a -> Abstract.PatternConstructorApp <$> goInfixPatternApplication a
  PatternPostfixApplication a -> Abstract.PatternConstructorApp <$> goPostfixPatternApplication a
  PatternWildcard i -> return (Abstract.PatternWildcard i)
  PatternEmpty {} -> return Abstract.PatternEmpty

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
