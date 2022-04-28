module MiniJuvix.Translation.AbstractToMicroJuvix
  ( module MiniJuvix.Translation.AbstractToMicroJuvix,
    module MiniJuvix.Syntax.MicroJuvix.MicroJuvixResult,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Abstract.AbstractResult qualified as Abstract
import MiniJuvix.Syntax.Abstract.Language.Extra qualified as A
import MiniJuvix.Syntax.Concrete.Name (symbolLoc)
import MiniJuvix.Syntax.Concrete.Scoped.Name qualified as S
import MiniJuvix.Syntax.MicroJuvix.Language
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixResult
import MiniJuvix.Syntax.Universe
import MiniJuvix.Syntax.Usage qualified as A

entryMicroJuvix ::
  Abstract.AbstractResult ->
  Sem r MicroJuvixResult
entryMicroJuvix ares = do
  _resultModules' <- mapM translateModule (ares ^. Abstract.resultModules)
  return
    MicroJuvixResult
      { _resultAbstract = ares,
        _resultModules = _resultModules'
      }

translateModule :: A.TopModule -> Sem r Module
translateModule m = do
  _moduleBody' <- goModuleBody (m ^. A.moduleBody)
  return
    Module
      { _moduleName = goTopModuleName (m ^. A.moduleName),
        _moduleBody = _moduleBody'
      }

goTopModuleName :: A.TopModuleName -> Name
goTopModuleName = goSymbol . S.topModulePathName

goName :: S.Name -> Name
goName = goSymbol . S.nameUnqualify

goSymbol :: S.Symbol -> Name
goSymbol s =
  Name
    { _nameText = S.symbolText s,
      _nameId = s ^. S.nameId,
      _nameKind = getNameKind s,
      _nameDefined = s ^. S.nameDefined,
      _nameLoc = s ^. S.nameConcrete . symbolLoc
    }

unsupported :: Text -> a
unsupported thing = error ("Abstract to MicroJuvix: Not yet supported: " <> thing)

goImport :: A.TopModule -> Sem r ModuleBody
goImport m = goModuleBody (m ^. A.moduleBody)

goModuleBody :: A.ModuleBody -> Sem r ModuleBody
goModuleBody b = ModuleBody <$> mapM goStatement (b ^. A.moduleStatements)

goStatement :: A.Statement -> Sem r Statement
goStatement = \case
  A.StatementAxiom d -> StatementAxiom <$> goAxiomDef d
  A.StatementForeign f -> return (StatementForeign f)
  A.StatementFunction f -> StatementFunction <$> goFunctionDef f
  A.StatementImport {} -> unsupported "imports"
  A.StatementLocalModule {} -> unsupported "local modules"
  A.StatementInductive i -> StatementInductive <$> goInductiveDef i
  A.StatementCompile c -> StatementCompile <$> goCompile c

goCompile :: A.Compile -> Sem r Compile
goCompile c = return (Compile nameSym backends)
  where
    nameSym = goSymbol (c ^. A.compileName)
    backends = c ^. A.compileBackendItems

goTypeIden :: A.Iden -> TypeIden
goTypeIden i = case i of
  A.IdenFunction {} -> unsupported "functions in types"
  A.IdenConstructor {} -> unsupported "constructors in types"
  A.IdenVar v -> TypeIdenVariable (goSymbol v)
  A.IdenInductive d -> TypeIdenInductive (goName (d ^. A.inductiveRefName))
  A.IdenAxiom a -> TypeIdenAxiom (goName (a ^. A.axiomRefName))

goAxiomDef :: A.AxiomDef -> Sem r AxiomDef
goAxiomDef a = do
  _axiomType' <- goType (a ^. A.axiomType)
  return
    AxiomDef
      { _axiomName = goSymbol (a ^. A.axiomName),
        _axiomType = _axiomType'
      }

goFunctionParameter :: A.FunctionParameter -> Sem r (Either VarName Type)
goFunctionParameter f = case f ^. A.paramName of
  Just var
    | isSmallType (f ^. A.paramType) && isOmegaUsage (f ^. A.paramUsage) -> return (Left (goSymbol var))
    | otherwise -> unsupported "named function arguments only for small types without usages"
  Nothing
    | isOmegaUsage (f ^. A.paramUsage) -> Right <$> goType (f ^. A.paramType)
    | otherwise -> unsupported "usages"

isOmegaUsage :: A.Usage -> Bool
isOmegaUsage u = case u of
  A.UsageOmega -> True
  _ -> False

goFunction :: A.Function -> Sem r Type
goFunction (A.Function l r) = do
  l' <- goFunctionParameter l
  r' <- goType r
  return $ case l' of
    Left tyvar -> TypeAbs (TypeAbstraction tyvar r')
    Right ty -> TypeFunction (Function ty r')

goFunctionDef :: A.FunctionDef -> Sem r FunctionDef
goFunctionDef f = do
  _funDefClauses' <- mapM (goFunctionClause _funDefName') (f ^. A.funDefClauses)
  _funDefType' <- goType (f ^. A.funDefTypeSig)
  return
    FunctionDef
      { _funDefName = _funDefName',
        _funDefType = _funDefType',
        _funDefClauses = _funDefClauses'
      }
  where
    _funDefName' :: Name
    _funDefName' = goSymbol (f ^. A.funDefName)

goFunctionClause :: Name -> A.FunctionClause -> Sem r FunctionClause
goFunctionClause n c = do
  _clauseBody' <- goExpression (c ^. A.clauseBody)
  _clausePatterns' <- mapM goPattern (c ^. A.clausePatterns)
  return
    FunctionClause
      { _clauseName = n,
        _clausePatterns = _clausePatterns',
        _clauseBody = _clauseBody'
      }

goPattern :: A.Pattern -> Sem r Pattern
goPattern p = case p of
  A.PatternVariable v -> return (PatternVariable (goSymbol v))
  A.PatternConstructorApp c -> PatternConstructorApp <$> goConstructorApp c
  A.PatternWildcard -> return PatternWildcard
  A.PatternEmpty -> unsupported "pattern empty"

goConstructorApp :: A.ConstructorApp -> Sem r ConstructorApp
goConstructorApp c = do
  _constrAppParameters' <- mapM goPattern (c ^. A.constrAppParameters)
  return
    ConstructorApp
      { _constrAppConstructor = goName (c ^. A.constrAppConstructor . A.constructorRefName),
        _constrAppParameters = _constrAppParameters'
      }

isSmallType :: A.Expression -> Bool
isSmallType e = case e of
  A.ExpressionUniverse u -> isSmallUni u
  _ -> False

isSmallUni :: Universe -> Bool
isSmallUni u = 0 == fromMaybe 0 (u ^. universeLevel)

goTypeUniverse :: Universe -> Type
goTypeUniverse u
  | isSmallUni u = TypeUniverse
  | otherwise = unsupported "big universes"

goType :: A.Expression -> Sem r Type
goType e = case e of
  A.ExpressionIden i -> return (TypeIden (goTypeIden i))
  A.ExpressionUniverse u -> return (goTypeUniverse u)
  A.ExpressionApplication a -> TypeApp <$> goTypeApplication a
  A.ExpressionFunction f -> goFunction f
  A.ExpressionLiteral {} -> unsupported "literals in types"

goApplication :: A.Application -> Sem r Application
goApplication (A.Application f x) = do
  f' <- goExpression f
  x' <- goExpression x
  return (Application f' x')

goIden :: A.Iden -> Iden
goIden i = case i of
  A.IdenFunction n -> IdenFunction (goName (n ^. A.functionRefName))
  A.IdenConstructor c -> IdenConstructor (goName (c ^. A.constructorRefName))
  A.IdenVar v -> IdenVar (goSymbol v)
  A.IdenAxiom a -> IdenAxiom (goName (a ^. A.axiomRefName))
  A.IdenInductive a -> IdenInductive (goName (a ^. A.inductiveRefName))

goExpression :: A.Expression -> Sem r Expression
goExpression e = case e of
  A.ExpressionIden i -> return (ExpressionIden (goIden i))
  A.ExpressionUniverse {} -> unsupported "universes in expression"
  A.ExpressionFunction {} -> unsupported "function type in expressions"
  A.ExpressionApplication a -> ExpressionApplication <$> goApplication a
  A.ExpressionLiteral l -> return (ExpressionLiteral l)

goInductiveParameter :: A.FunctionParameter -> Sem r InductiveParameter
goInductiveParameter f =
  case (f ^. A.paramName, f ^. A.paramUsage, f ^. A.paramType) of
    (Just var, A.UsageOmega, A.ExpressionUniverse u)
      | isSmallUni u ->
          return
            InductiveParameter
              { _inductiveParamName = goSymbol var
              }
    (Just {}, _, _) -> unsupported "only type variables of small types are allowed"
    (Nothing, _, _) -> unsupported "unnamed inductive parameters"

goInductiveDef :: forall r. A.InductiveDef -> Sem r InductiveDef
goInductiveDef i = case i ^. A.inductiveType of
  Just {} -> unsupported "inductive indices"
  _ -> do
    _inductiveParameters' <- mapM goInductiveParameter (i ^. A.inductiveParameters)
    _inductiveConstructors' <- mapM goConstructorDef (i ^. A.inductiveConstructors)
    return
      InductiveDef
        { _inductiveName = indName,
          _inductiveParameters = _inductiveParameters',
          _inductiveConstructors = _inductiveConstructors'
        }
  where
    indName = goSymbol (i ^. A.inductiveName)
    goConstructorDef :: A.InductiveConstructorDef -> Sem r InductiveConstructorDef
    goConstructorDef c = do
      _constructorParameters' <- goConstructorType (c ^. A.constructorType)
      return
        InductiveConstructorDef
          { _constructorName = goSymbol (c ^. A.constructorName),
            _constructorParameters = _constructorParameters'
          }
    -- TODO check that the return type corresponds with the inductive type
    goConstructorType :: A.Expression -> Sem r [Type]
    goConstructorType = fmap fst . viewConstructorType

goTypeApplication :: A.Application -> Sem r TypeApplication
goTypeApplication (A.Application l r) = do
  l' <- goType l
  r' <- goType r
  return
    TypeApplication
      { _typeAppLeft = l',
        _typeAppRight = r'
      }

viewConstructorType :: A.Expression -> Sem r ([Type], Type)
viewConstructorType e = case e of
  A.ExpressionFunction f -> first toList <$> viewFunctionType f
  A.ExpressionIden i -> return ([], TypeIden (goTypeIden i))
  A.ExpressionApplication a -> do
    a' <- goTypeApplication a
    return ([], TypeApp a')
  A.ExpressionUniverse {} -> return ([], TypeUniverse)
  A.ExpressionLiteral {} -> unsupported "literal in a type"
  where
    viewFunctionType :: A.Function -> Sem r (NonEmpty Type, Type)
    viewFunctionType (A.Function p r) = do
      (args, ret) <- viewConstructorType r
      p' <- goFunctionParameter p
      return $ case p' of
        Left {} -> unsupported "type abstraction in constructor type"
        Right ty -> (ty :| args, ret)
