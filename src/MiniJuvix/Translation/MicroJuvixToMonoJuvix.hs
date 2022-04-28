module MiniJuvix.Translation.MicroJuvixToMonoJuvix
  ( module MiniJuvix.Translation.MicroJuvixToMonoJuvix,
    module MiniJuvix.Syntax.MonoJuvix.MonoJuvixResult,
  )
where

import Data.Text qualified as Text
import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.InfoTable qualified as Micro
import MiniJuvix.Syntax.MicroJuvix.Language qualified as Micro
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixTypedResult qualified as Micro
import MiniJuvix.Syntax.MonoJuvix.Language
import MiniJuvix.Syntax.MonoJuvix.MonoJuvixResult
import MiniJuvix.Syntax.NameId

entryMonoJuvix ::
  Member (Error Err) r =>
  Micro.MicroJuvixTypedResult ->
  Sem r MonoJuvixResult
entryMonoJuvix i = do
  _resultModules <- mapM goModule' (i ^. Micro.resultModules)
  return MonoJuvixResult {..}
  where
    _resultMicroTyped = i
    goModule' m = runReader table (goModule m)
      where
        table = Micro.buildTable m

translateModule :: Micro.Module -> Either Err Module
translateModule m = run (runError (runReader table (goModule m)))
  where
    table = Micro.buildTable m

type Err = Text

goModule :: Members '[Error Err, Reader Micro.InfoTable] r => Micro.Module -> Sem r Module
goModule Micro.Module {..} = do
  _moduleBody' <- goModuleBody _moduleBody
  return
    Module
      { _moduleName = goName _moduleName,
        _moduleBody = _moduleBody'
      }

unsupported :: Text -> a
unsupported msg = error $ msg <> " not yet supported"

goModuleBody ::
  Members '[Error Err, Reader Micro.InfoTable] r =>
  Micro.ModuleBody ->
  Sem r ModuleBody
goModuleBody Micro.ModuleBody {..} =
  ModuleBody <$> mapM goStatement _moduleStatements

goStatement :: Members '[Error Err, Reader Micro.InfoTable] r => Micro.Statement -> Sem r Statement
goStatement = \case
  Micro.StatementInductive d -> StatementInductive <$> goInductive d
  Micro.StatementFunction d -> StatementFunction <$> goFunctionDef d
  Micro.StatementForeign d -> return (StatementForeign d)
  Micro.StatementAxiom a -> StatementAxiom <$> goAxiomDef a
  Micro.StatementCompile a -> StatementCompile <$> goCompile a

goCompile :: Micro.Compile -> Sem r Compile
goCompile Micro.Compile {..} = do
  return Compile {_compileName = goName _compileName, ..}

goAxiomDef :: Members '[Error Err, Reader Micro.InfoTable] r => Micro.AxiomDef -> Sem r AxiomDef
goAxiomDef Micro.AxiomDef {..} = do
  _axiomType' <- goType _axiomType
  return
    AxiomDef
      { _axiomName = goName _axiomName,
        _axiomType = _axiomType'
      }

lookupAxiom :: Members '[Error Err, Reader Micro.InfoTable] r => Micro.Name -> Sem r Micro.AxiomInfo
lookupAxiom n =
  fromMaybe impossible . (^. Micro.infoAxioms . at n) <$> ask

goIden :: Micro.Iden -> Sem r Iden
goIden = \case
  Micro.IdenFunction fun -> return (IdenFunction (goName fun))
  Micro.IdenConstructor c -> return (IdenConstructor (goName c))
  Micro.IdenVar v -> return (IdenVar (goName v))
  Micro.IdenAxiom a -> return (IdenAxiom (goName a))

throwErr :: Member (Error Err) r => Text -> Sem r a
throwErr = throw

goName :: Micro.Name -> Name
goName n =
  Name
    { _nameText = goNameText n,
      _nameId = n ^. Micro.nameId,
      _nameDefined = Just (n ^. Micro.nameDefined),
      _nameLoc = Just (n ^. Micro.nameLoc),
      _nameKind = n ^. Micro.nameKind
    }

goNameText :: Micro.Name -> Text
goNameText n =
  adaptFirstLetter lexeme <> nameTextSuffix
  where
    lexeme
      | Text.null lexeme' = "v"
      | otherwise = lexeme'
      where
        lexeme' = Text.filter isValidChar (n ^. Micro.nameText)
    isValidChar :: Char -> Bool
    isValidChar c = isLetter c && isAscii c
    adaptFirstLetter :: Text -> Text
    adaptFirstLetter t = case Text.uncons t of
      Nothing -> impossible
      Just (h, r) -> Text.cons (capitalize h) r
      where
        capitalize :: Char -> Char
        capitalize
          | capital = toUpper
          | otherwise = toLower
        capital = case n ^. Micro.nameKind of
          KNameConstructor -> True
          KNameInductive -> True
          KNameTopModule -> True
          KNameLocalModule -> True
          _ -> False
    nameTextSuffix :: Text
    nameTextSuffix = case n ^. Micro.nameKind of
      KNameTopModule -> mempty
      KNameFunction ->
        if n ^. Micro.nameText == haskellMainName then mempty else idSuffix
      _ -> idSuffix
    idSuffix :: Text
    idSuffix = "_" <> show (n ^. Micro.nameId . unNameId)
    haskellMainName :: Text
    haskellMainName = "main"

goFunctionDef :: Members '[Error Err, Reader Micro.InfoTable] r => Micro.FunctionDef -> Sem r FunctionDef
goFunctionDef Micro.FunctionDef {..} = do
  _funDefType' <- goType _funDefType
  _funDefClauses' <- mapM goFunctionClause _funDefClauses
  return
    FunctionDef
      { _funDefName = goName _funDefName,
        _funDefType = _funDefType',
        _funDefClauses = _funDefClauses'
      }

goPattern :: Micro.Pattern -> Pattern
goPattern = \case
  Micro.PatternVariable v -> PatternVariable (goName v)
  Micro.PatternConstructorApp a -> PatternConstructorApp (goConstructorApp a)
  Micro.PatternWildcard -> PatternWildcard

goConstructorApp :: Micro.ConstructorApp -> ConstructorApp
goConstructorApp c =
  ConstructorApp
    { _constrAppConstructor = goName (c ^. Micro.constrAppConstructor),
      _constrAppParameters = map goPattern (c ^. Micro.constrAppParameters)
    }

goExpression ::
  Members '[Error Err, Reader Micro.InfoTable] r =>
  Micro.Expression ->
  Sem r Expression
goExpression = \case
  Micro.ExpressionIden i -> ExpressionIden <$> goIden i
  Micro.ExpressionTyped t -> goExpression (t ^. Micro.typedExpression)
  Micro.ExpressionApplication a -> ExpressionApplication <$> goApplication a
  Micro.ExpressionLiteral l -> return (ExpressionLiteral l)

goApplication ::
  Members '[Error Err, Reader Micro.InfoTable] r =>
  Micro.Application ->
  Sem r Application
goApplication Micro.Application {..} = do
  _appLeft' <- goExpression _appLeft
  _appRight' <- goExpression _appRight
  return
    Application
      { _appLeft = _appLeft',
        _appRight = _appRight'
      }

goFunctionClause ::
  Members '[Error Err, Reader Micro.InfoTable] r =>
  Micro.FunctionClause ->
  Sem r FunctionClause
goFunctionClause Micro.FunctionClause {..} = do
  _clauseBody' <- goExpression _clauseBody
  let _clausePatterns' = map goPattern _clausePatterns
  return
    FunctionClause
      { _clauseBody = _clauseBody',
        _clauseName = goName _clauseName,
        _clausePatterns = _clausePatterns'
      }

goInductive ::
  Members '[Error Err, Reader Micro.InfoTable] r =>
  Micro.InductiveDef ->
  Sem r InductiveDef
goInductive Micro.InductiveDef {..} = do
  _inductiveConstructors' <- mapM goConstructorDef _inductiveConstructors
  return
    InductiveDef
      { _inductiveName = goName _inductiveName,
        _inductiveConstructors = _inductiveConstructors'
      }

goConstructorDef ::
  Members '[Error Err, Reader Micro.InfoTable] r =>
  Micro.InductiveConstructorDef ->
  Sem r InductiveConstructorDef
goConstructorDef Micro.InductiveConstructorDef {..} = do
  _constructorParameters' <- mapM goType _constructorParameters
  return
    InductiveConstructorDef
      { _constructorName = goName _constructorName,
        _constructorParameters = _constructorParameters'
      }

goFunction :: Members '[Error Err, Reader Micro.InfoTable] r => Micro.Function -> Sem r Function
goFunction Micro.Function {..} = do
  _funLeft' <- goType _funLeft
  _funRight' <- goType _funRight
  return
    Function
      { _funLeft = _funLeft',
        _funRight = _funRight'
      }

goTypeIden :: Members '[Error Err, Reader Micro.InfoTable] r => Micro.TypeIden -> Sem r Type
goTypeIden = \case
  Micro.TypeIdenInductive n -> return (TypeIden (TypeIdenInductive (goName n)))
  Micro.TypeIdenAxiom n -> return (TypeIden (TypeIdenAxiom (goName n)))

goType :: Members '[Error Err, Reader Micro.InfoTable] r => Micro.Type -> Sem r Type
goType = \case
  Micro.TypeIden t -> goTypeIden t
  Micro.TypeFunction f -> TypeFunction <$> goFunction f
  Micro.TypeUniverse -> throwErr "MonoJuvix: universes in types not supported"
  Micro.TypeAny -> impossible
