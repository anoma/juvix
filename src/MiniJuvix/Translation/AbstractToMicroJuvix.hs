module MiniJuvix.Translation.AbstractToMicroJuvix
  ( module MiniJuvix.Translation.AbstractToMicroJuvix,
    module MiniJuvix.Syntax.MicroJuvix.MicroJuvixResult,
    module MiniJuvix.Termination.Checker,
  )
where

import Data.HashSet qualified as HashSet
import MiniJuvix.Pipeline.EntryPoint qualified as E
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Abstract.AbstractResult qualified as Abstract
import MiniJuvix.Syntax.Abstract.Language qualified as Abstract
import MiniJuvix.Syntax.MicroJuvix.Error
import MiniJuvix.Syntax.MicroJuvix.Language
import MiniJuvix.Syntax.MicroJuvix.Language.Extra
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixResult
import MiniJuvix.Syntax.Universe
import MiniJuvix.Syntax.Usage
import MiniJuvix.Termination.Checker

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

entryMicroJuvix ::
  Members '[Error MiniJuvixError] r =>
  Abstract.AbstractResult ->
  Sem r MicroJuvixResult
entryMicroJuvix abstractResults = do
  unless
    noTerminationOption
    ( mapError
        (MiniJuvixError @TerminationError)
        (checkTermination topModule infoTable)
    )
  _resultModules' <-
    evalState
      iniState
      ( mapM
          (mapError (MiniJuvixError @TypeCheckerError) . goModule)
          (abstractResults ^. Abstract.resultModules)
      )
  return
    MicroJuvixResult
      { _resultAbstract = abstractResults,
        _resultModules = _resultModules'
      }
  where
    topModule = head (abstractResults ^. Abstract.resultModules)
    infoTable = abstractResults ^. Abstract.resultTable
    noTerminationOption =
      abstractResults
        ^. Abstract.abstractResultEntryPoint
          . E.entryPointNoTermination

goModule ::
  Members '[State TranslationState, Error TypeCheckerError] r =>
  Abstract.TopModule ->
  Sem r Module
goModule m = do
  _moduleBody' <- goModuleBody (m ^. Abstract.moduleBody)
  return
    Module
      { _moduleName = m ^. Abstract.moduleName,
        _moduleBody = _moduleBody'
      }

unsupported :: Text -> a
unsupported thing = error ("Abstract to MicroJuvix: Not yet supported: " <> thing)

goModuleBody :: Members '[State TranslationState, Error TypeCheckerError] r => Abstract.ModuleBody -> Sem r ModuleBody
goModuleBody b = ModuleBody <$> mapMaybeM goStatement (b ^. Abstract.moduleStatements)

goImport :: Members '[State TranslationState, Error TypeCheckerError] r => Abstract.TopModule -> Sem r (Maybe Include)
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
  Members [State TranslationState, Error TypeCheckerError] r =>
  Abstract.Statement ->
  Sem r (Maybe Statement)
goStatement = \case
  Abstract.StatementAxiom d -> Just . StatementAxiom <$> goAxiomDef d
  Abstract.StatementForeign f -> return (Just (StatementForeign f))
  Abstract.StatementFunction f -> Just . StatementFunction <$> goFunctionDef f
  Abstract.StatementImport i -> fmap StatementInclude <$> goImport i
  Abstract.StatementLocalModule {} -> unsupported "local modules"
  Abstract.StatementInductive i -> Just . StatementInductive <$> goInductiveDef i

goTypeIden :: Abstract.Iden -> TypeIden
goTypeIden = \case
  Abstract.IdenFunction {} -> unsupported "functions in types"
  Abstract.IdenConstructor {} -> unsupported "constructors in types"
  Abstract.IdenVar v -> TypeIdenVariable v
  Abstract.IdenInductive d -> TypeIdenInductive (d ^. Abstract.inductiveRefName)
  Abstract.IdenAxiom a -> TypeIdenAxiom (a ^. Abstract.axiomRefName)

goAxiomDef :: Abstract.AxiomDef -> Sem r AxiomDef
goAxiomDef a = do
  _axiomType' <- goType (a ^. Abstract.axiomType)
  return
    AxiomDef
      { _axiomName = a ^. Abstract.axiomName,
        _axiomType = _axiomType'
      }

goFunctionParameter :: Abstract.FunctionParameter -> Sem r (Either VarName Type)
goFunctionParameter f = case f ^. Abstract.paramName of
  Just var
    | isSmallType (f ^. Abstract.paramType) && isOmegaUsage (f ^. Abstract.paramUsage) ->
        return (Left var)
    | otherwise -> unsupported "named function arguments only for small types without usages"
  Nothing
    | isOmegaUsage (f ^. Abstract.paramUsage) -> Right <$> goType (f ^. Abstract.paramType)
    | otherwise -> unsupported "usages"

isOmegaUsage :: Usage -> Bool
isOmegaUsage u = case u of
  UsageOmega -> True
  _ -> False

goFunction :: Abstract.Function -> Sem r Type
goFunction (Abstract.Function l r) = do
  l' <- goFunctionParameter l
  r' <- goType r
  return $ case l' of
    Left tyVar ->
      TypeAbs
        ( TypeAbstraction
            { _typeAbsVar = tyVar,
              _typeAbsImplicit = l ^. Abstract.paramImplicit,
              _typeAbsBody = r'
            }
        )
    Right ty -> TypeFunction (Function ty r')

goFunctionDef :: Abstract.FunctionDef -> Sem r FunctionDef
goFunctionDef f = do
  _funDefClauses' <- mapM (goFunctionClause _funDefName') (f ^. Abstract.funDefClauses)
  _funDefType' <- goType (f ^. Abstract.funDefTypeSig)
  return
    FunctionDef
      { _funDefName = _funDefName',
        _funDefType = _funDefType',
        _funDefClauses = _funDefClauses'
      }
  where
    _funDefName' :: Name
    _funDefName' = f ^. Abstract.funDefName

goFunctionClause :: Name -> Abstract.FunctionClause -> Sem r FunctionClause
goFunctionClause n c = do
  _clauseBody' <- goExpression (c ^. Abstract.clauseBody)
  _clausePatterns' <- mapM goPattern (c ^. Abstract.clausePatterns)
  return
    FunctionClause
      { _clauseName = n,
        _clausePatterns = _clausePatterns',
        _clauseBody = _clauseBody'
      }

goPattern :: Abstract.Pattern -> Sem r Pattern
goPattern p = case p of
  Abstract.PatternVariable v -> return (PatternVariable v)
  Abstract.PatternConstructorApp c -> PatternConstructorApp <$> goConstructorApp c
  Abstract.PatternWildcard i -> return (PatternWildcard i)
  Abstract.PatternBraces b -> PatternBraces <$> goPattern b
  Abstract.PatternEmpty -> unsupported "pattern empty"

goConstructorApp :: Abstract.ConstructorApp -> Sem r ConstructorApp
goConstructorApp c = do
  _constrAppParameters' <- mapM goPattern (c ^. Abstract.constrAppParameters)
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

goTypeUniverse :: Universe -> Type
goTypeUniverse u
  | isSmallUni u = TypeUniverse
  | otherwise = unsupported "big universes"

goType :: Abstract.Expression -> Sem r Type
goType e = case e of
  Abstract.ExpressionIden i -> return (TypeIden (goTypeIden i))
  Abstract.ExpressionUniverse u -> return (goTypeUniverse u)
  Abstract.ExpressionApplication a -> TypeApp <$> goTypeApplication a
  Abstract.ExpressionFunction f -> goFunction f
  Abstract.ExpressionLiteral {} -> unsupported "literals in types"
  Abstract.ExpressionHole h -> return (TypeHole h)

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

goExpressionFunction :: forall r. Abstract.Function -> Sem r FunctionExpression
goExpressionFunction f = do
  l' <- goParam (f ^. Abstract.funParameter)
  r' <- goExpression (f ^. Abstract.funReturn)
  return (FunctionExpression l' r')
  where
    goParam :: Abstract.FunctionParameter -> Sem r Expression
    goParam p
      | isJust (p ^. Abstract.paramName) = unsupported "named type parameters"
      | isOmegaUsage (p ^. Abstract.paramUsage) = goExpression (p ^. Abstract.paramType)
      | otherwise = unsupported "usages"

goExpression :: Abstract.Expression -> Sem r Expression
goExpression e = case e of
  Abstract.ExpressionIden i -> return (ExpressionIden (goIden i))
  Abstract.ExpressionUniverse {} -> unsupported "universes in expression"
  Abstract.ExpressionFunction f -> ExpressionFunction <$> goExpressionFunction f
  Abstract.ExpressionApplication a -> ExpressionApplication <$> goApplication a
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

goConstructorType :: Abstract.Expression -> Sem r [Type]
goConstructorType = fmap fst . viewConstructorType

goInductiveDef ::
  forall r.
  Members '[Error TypeCheckerError] r =>
  Abstract.InductiveDef ->
  Sem r InductiveDef
goInductiveDef i = case i ^. Abstract.inductiveType of
  Just Abstract.ExpressionUniverse {} -> helper
  Just {} -> unsupported "inductive indices"
  _ -> helper
  where
    indTypeName = i ^. Abstract.inductiveName
    helper = do
      inductiveParameters' <- mapM goInductiveParameter (i ^. Abstract.inductiveParameters)
      let indTy :: Type = foldTypeAppName indTypeName (map (^. inductiveParamName) inductiveParameters')
      inductiveConstructors' <- mapM (goConstructorDef indTy) (i ^. Abstract.inductiveConstructors)
      return
        InductiveDef
          { _inductiveName = indTypeName,
            _inductiveParameters = inductiveParameters',
            _inductiveConstructors = inductiveConstructors'
          }
      where
        goConstructorDef :: Type -> Abstract.InductiveConstructorDef -> Sem r InductiveConstructorDef
        goConstructorDef expectedReturnType c = do
          (_constructorParameters', actualReturnType) <- viewConstructorType (c ^. Abstract.constructorType)
          let ctorName = c ^. Abstract.constructorName
          if
              | actualReturnType == expectedReturnType ->
                  return
                    InductiveConstructorDef
                      { _constructorName = ctorName,
                        _constructorParameters = _constructorParameters'
                      }
              | otherwise ->
                  throw
                    ( ErrWrongReturnType
                        (WrongReturnType ctorName expectedReturnType actualReturnType)
                    )

goTypeApplication :: Abstract.Application -> Sem r TypeApplication
goTypeApplication (Abstract.Application l r i) = do
  l' <- goType l
  r' <- goType r
  return
    TypeApplication
      { _typeAppLeft = l',
        _typeAppRight = r',
        _typeAppImplicit = i
      }

viewConstructorType :: Abstract.Expression -> Sem r ([Type], Type)
viewConstructorType = \case
  Abstract.ExpressionFunction f -> first toList <$> viewFunctionType f
  Abstract.ExpressionIden i -> return ([], TypeIden (goTypeIden i))
  Abstract.ExpressionHole {} -> unsupported "holes in constructor type"
  Abstract.ExpressionApplication a -> do
    a' <- goTypeApplication a
    return ([], TypeApp a')
  Abstract.ExpressionUniverse {} -> return ([], TypeUniverse)
  Abstract.ExpressionLiteral {} -> unsupported "literal in a type"
  where
    viewFunctionType :: Abstract.Function -> Sem r (NonEmpty Type, Type)
    viewFunctionType (Abstract.Function p r) = do
      (args, ret) <- viewConstructorType r
      p' <- goFunctionParameter p
      return $ case p' of
        Left {} -> unsupported "type abstraction in constructor type"
        Right ty -> (ty :| args, ret)
