module MiniJuvix.Translation.AbstractToMicroJuvix where

import MiniJuvix.Prelude
import qualified MiniJuvix.Syntax.Abstract.Language.Extra as A
import qualified MiniJuvix.Syntax.Concrete.Scoped.Name as S
import MiniJuvix.Syntax.MicroJuvix.Language
import MiniJuvix.Syntax.Universe
import qualified MiniJuvix.Syntax.Usage as A
import MiniJuvix.Syntax.Concrete.Name (symbolLoc)

translateModule :: A.TopModule -> Module
translateModule m =
  Module
    { _moduleName = goTopModuleName (m ^. A.moduleName),
      _moduleBody = goModuleBody (m ^. A.moduleBody)
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
      _nameLoc = s ^. S.nameConcrete . symbolLoc }


unsupported :: Text -> a
unsupported thing = error ("Abstract to MicroJuvix: Not yet supported: " <> thing)

goImport :: A.TopModule -> ModuleBody
goImport m = goModuleBody (m ^. A.moduleBody)

goModuleBody :: A.ModuleBody -> ModuleBody
goModuleBody b = ModuleBody (map goStatement (b ^. A.moduleStatements))

goStatement :: A.Statement -> Statement
goStatement = \case
  A.StatementAxiom d -> StatementAxiom (goAxiomDef d)
  A.StatementForeign f -> StatementForeign f
  A.StatementFunction f -> StatementFunction (goFunctionDef f)
  A.StatementImport {} -> unsupported "imports"
  A.StatementLocalModule {} -> unsupported "local modules"
  A.StatementInductive i -> StatementInductive (goInductiveDef i)

goTypeIden :: A.Iden -> TypeIden
goTypeIden i = case i of
  A.IdenFunction {} -> unsupported "functions in types"
  A.IdenConstructor {} -> unsupported "constructors in types"
  A.IdenVar {} -> unsupported "type variables"
  A.IdenInductive d -> TypeIdenInductive (goName (d ^. A.inductiveRefName))
  A.IdenAxiom a -> TypeIdenAxiom (goName (a ^. A.axiomRefName))

goAxiomDef :: A.AxiomDef -> AxiomDef
goAxiomDef a =
  AxiomDef {
  _axiomName = goSymbol (a ^. A.axiomName),
  _axiomType = goType (a ^. A.axiomType),
  _axiomBackendItems = a ^. A.axiomBackendItems }

goFunctionParameter :: A.FunctionParameter -> Type
goFunctionParameter f = case f ^. A.paramName of
  Just {} -> unsupported "named function arguments"
  _ -> case f ^. A.paramUsage of
    A.UsageOmega -> goType (f ^. A.paramType)
    _ -> unsupported "usages"

goFunction :: A.Function -> Function
goFunction (A.Function l r) = Function (goFunctionParameter l) (goType r)

goFunctionDef :: A.FunctionDef -> FunctionDef
goFunctionDef f =
  FunctionDef
    { _funDefName = _funDefName',
      _funDefType = goType (f ^. A.funDefTypeSig),
      _funDefClauses = fmap (goFunctionClause _funDefName') (f ^. A.funDefClauses)
    }
  where
    _funDefName' :: Name
    _funDefName' = goSymbol (f ^. A.funDefName)

goFunctionClause :: Name -> A.FunctionClause -> FunctionClause
goFunctionClause n c =
  FunctionClause
    { _clauseName = n,
      _clausePatterns = map goPattern (c ^. A.clausePatterns),
      _clauseBody = goExpression (c ^. A.clauseBody)
    }

goPattern :: A.Pattern -> Pattern
goPattern p = case p of
  A.PatternVariable v -> PatternVariable (goSymbol v)
  A.PatternConstructorApp c -> PatternConstructorApp (goConstructorApp c)
  A.PatternWildcard -> PatternWildcard
  A.PatternEmpty -> unsupported "pattern empty"

goConstructorApp :: A.ConstructorApp -> ConstructorApp
goConstructorApp c =
  ConstructorApp
    (goName (c ^. A.constrAppConstructor . A.constructorRefName))
    (map goPattern (c ^. A.constrAppParameters))

goTypeUniverse :: Universe -> Type
goTypeUniverse u
 | 0 == fromMaybe 0 (u ^. universeLevel) = TypeUniverse
 | otherwise = unsupported "big universes"

goType :: A.Expression -> Type
goType e = case e of
  A.ExpressionIden i -> TypeIden (goTypeIden i)
  A.ExpressionUniverse u -> goTypeUniverse u
  A.ExpressionApplication {} -> unsupported "application in types"
  A.ExpressionFunction f -> TypeFunction (goFunction f)
  A.ExpressionLiteral {} -> unsupported "literals in types"

goApplication :: A.Application -> Application
goApplication (A.Application f x) = Application (goExpression f) (goExpression x)

goIden :: A.Iden -> Iden
goIden i = case i of
  A.IdenFunction n -> IdenFunction (goName (n ^. A.functionRefName))
  A.IdenConstructor c -> IdenConstructor (goName (c ^. A.constructorRefName))
  A.IdenVar v -> IdenVar (goSymbol v)
  A.IdenAxiom a -> IdenAxiom (goName (a ^. A.axiomRefName))
  A.IdenInductive {} -> unsupported "inductive identifier"

goExpression :: A.Expression -> Expression
goExpression e = case e of
  A.ExpressionIden i -> ExpressionIden (goIden i)
  A.ExpressionUniverse {} -> unsupported "universes in expression"
  A.ExpressionFunction {} -> unsupported "function type in expressions"
  A.ExpressionApplication a -> ExpressionApplication (goApplication a)
  A.ExpressionLiteral l -> ExpressionLiteral l

goInductiveDef :: A.InductiveDef -> InductiveDef
goInductiveDef i = case i ^. A.inductiveType of
  Just {} -> unsupported "inductive indices"
  _ ->
    InductiveDef
      { _inductiveName = indName,
        _inductiveConstructors = map goConstructorDef (i ^. A.inductiveConstructors)
      }
  where
    indName = goSymbol (i ^. A.inductiveName)
    goConstructorDef :: A.InductiveConstructorDef -> InductiveConstructorDef
    goConstructorDef c =
      InductiveConstructorDef
        { _constructorName = goSymbol (c ^. A.constructorName),
          _constructorParameters = goConstructorType (c ^. A.constructorType)
        }
    goConstructorType :: A.Expression -> [Type]
    goConstructorType = fst . viewExpressionFunctionType

-- TODO: add docs or an example
viewExpressionFunctionType :: A.Expression -> ([Type], Type)
viewExpressionFunctionType e = case e of
  A.ExpressionFunction f -> first toList (viewFunctionType f)
  A.ExpressionIden i -> ([], TypeIden (goTypeIden i))
  A.ExpressionApplication {} -> unsupported "application in a type"
  A.ExpressionUniverse {} -> ([], TypeUniverse)
  A.ExpressionLiteral {} -> unsupported "literal in a type"

viewFunctionType :: A.Function -> (NonEmpty Type, Type)
viewFunctionType (A.Function p r) = (goFunctionParameter p :| args, ret)
  where
    (args, ret) = viewExpressionFunctionType r
