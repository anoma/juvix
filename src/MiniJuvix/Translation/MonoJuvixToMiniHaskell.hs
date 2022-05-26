module MiniJuvix.Translation.MonoJuvixToMiniHaskell
  ( module MiniJuvix.Translation.MonoJuvixToMiniHaskell,
    module MiniJuvix.Syntax.MiniHaskell.MiniHaskellResult,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as Text
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Backends
import MiniJuvix.Syntax.Concrete.Scoped.InfoTable qualified as S
import MiniJuvix.Syntax.ForeignBlock
import MiniJuvix.Syntax.MiniHaskell.Language
import MiniJuvix.Syntax.MiniHaskell.MiniHaskellResult
import MiniJuvix.Syntax.MonoJuvix.Language qualified as Mono
import MiniJuvix.Syntax.MonoJuvix.MonoJuvixResult qualified as Mono
import Prettyprinter

entryMiniHaskell ::
  Mono.MonoJuvixResult ->
  Sem r MiniHaskellResult
entryMiniHaskell i = do
  _resultModules <- mapM goModule' (i ^. Mono.resultModules)
  return MiniHaskellResult {..}
  where
    _resultMonoJuvix = i
    goModule' m = runReader compileTable (goModule m)
      where
        compileTable :: Mono.CompileInfoTable
        compileTable = Mono.compileInfoTable i

goModule :: Members '[Reader Mono.CompileInfoTable] r => Mono.Module -> Sem r Module
goModule Mono.Module {..} = do
  _moduleBody' <- goModuleBody _moduleBody
  return
    Module
      { _moduleName = goName _moduleName,
        _moduleBody = _moduleBody'
      }

unsupported :: Text -> a
unsupported msg = error $ msg <> " not yet supported"

goModuleBody ::
  Members '[Reader Mono.CompileInfoTable] r =>
  Mono.ModuleBody ->
  Sem r ModuleBody
goModuleBody Mono.ModuleBody {..} =
  ModuleBody <$> mapMaybeM goStatement _moduleStatements

goStatement :: Members '[Reader Mono.CompileInfoTable] r => Mono.Statement -> Sem r (Maybe Statement)
goStatement = \case
  Mono.StatementInductive d -> Just . StatementInductive <$> goInductive d
  Mono.StatementFunction d -> Just . StatementFunction <$> goFunctionDef d
  Mono.StatementForeign d -> return (goForeign d)
  Mono.StatementAxiom {} -> return Nothing

goForeign :: ForeignBlock -> Maybe Statement
goForeign b = case b ^. foreignBackend of
  BackendGhc -> Just (StatementVerbatim (b ^. foreignCode))
  _ -> Nothing

goIden :: Members '[Reader Mono.CompileInfoTable] r => Mono.Iden -> Sem r Expression
goIden = \case
  Mono.IdenFunction fun -> return (goName' fun)
  Mono.IdenConstructor c -> return (goName' c)
  Mono.IdenVar v -> return (goName' v)
  Mono.IdenAxiom a -> ExpressionVerbatim <$> goAxiomIden a

goAxiomIden :: Members '[Reader Mono.CompileInfoTable] r => Mono.Name -> Sem r Text
goAxiomIden n = do
  backends <- lookupBackends (n ^. Mono.nameId)
  case firstJust getCode backends of
    Nothing -> error ("ghc does not support this primitive:" <> show (pretty n))
    Just t -> return t
  where
    getCode :: BackendItem -> Maybe Text
    getCode b =
      guard (BackendGhc == b ^. backendItemBackend)
        $> b ^. backendItemCode
    lookupBackends ::
      Member (Reader Mono.CompileInfoTable) r =>
      NameId ->
      Sem r [BackendItem]
    lookupBackends f = (^. S.compileInfoBackendItems) . HashMap.lookupDefault impossible f <$> ask

goName' :: Mono.Name -> Expression
goName' = ExpressionIden . goName

goName :: Mono.Name -> Name
goName n =
  Name
    { _nameText = goNameText n,
      _nameKind = n ^. Mono.nameKind
    }

goNameText :: Mono.Name -> Text
goNameText n =
  adaptFirstLetter lexeme <> nameTextSuffix
  where
    lexeme
      | Text.null lexeme' = "v"
      | otherwise = lexeme'
      where
        lexeme' = Text.filter isValidChar (n ^. Mono.nameText)
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
        capital = case n ^. Mono.nameKind of
          KNameConstructor -> True
          KNameInductive -> True
          KNameTopModule -> True
          KNameLocalModule -> True
          _ -> False
    nameTextSuffix :: Text
    nameTextSuffix = case n ^. Mono.nameKind of
      KNameTopModule -> mempty
      KNameFunction ->
        if n ^. Mono.nameText == haskellMainName then mempty else idSuffix
      _ -> idSuffix
    idSuffix = "_" <> show (n ^. Mono.nameId . unNameId)
    haskellMainName = "main" :: Text

goFunctionDef :: Members '[Reader Mono.CompileInfoTable] r => Mono.FunctionDef -> Sem r FunctionDef
goFunctionDef Mono.FunctionDef {..} = do
  _funDefType' <- goType _funDefType
  _funDefClauses' <- mapM goFunctionClause _funDefClauses
  return
    FunctionDef
      { _funDefName = goName _funDefName,
        _funDefType = _funDefType',
        _funDefClauses = _funDefClauses'
      }

goPattern :: Mono.Pattern -> Pattern
goPattern = \case
  Mono.PatternVariable v -> PatternVariable (goName v)
  Mono.PatternConstructorApp a -> PatternConstructorApp (goConstructorApp a)
  Mono.PatternWildcard -> PatternWildcard

goConstructorApp :: Mono.ConstructorApp -> ConstructorApp
goConstructorApp c =
  ConstructorApp
    { _constrAppConstructor = goName (c ^. Mono.constrAppConstructor),
      _constrAppParameters = map goPattern (c ^. Mono.constrAppParameters)
    }

goExpression ::
  Members '[Reader Mono.CompileInfoTable] r =>
  Mono.Expression ->
  Sem r Expression
goExpression = \case
  Mono.ExpressionIden i -> goIden i
  Mono.ExpressionApplication a -> ExpressionApplication <$> goApplication a
  Mono.ExpressionLiteral l -> return (ExpressionLiteral l)

goApplication ::
  Members '[Reader Mono.CompileInfoTable] r =>
  Mono.Application ->
  Sem r Application
goApplication Mono.Application {..} = do
  _appLeft' <- goExpression _appLeft
  _appRight' <- goExpression _appRight
  return
    Application
      { _appLeft = _appLeft',
        _appRight = _appRight'
      }

goFunctionClause ::
  Members '[Reader Mono.CompileInfoTable] r =>
  Mono.FunctionClause ->
  Sem r FunctionClause
goFunctionClause Mono.FunctionClause {..} = do
  _clauseBody' <- goExpression _clauseBody
  let _clausePatterns' = map goPattern _clausePatterns
  return
    FunctionClause
      { _clauseBody = _clauseBody',
        _clausePatterns = _clausePatterns'
      }

goInductive ::
  Members '[Reader Mono.CompileInfoTable] r =>
  Mono.InductiveDef ->
  Sem r InductiveDef
goInductive Mono.InductiveDef {..} = do
  _inductiveConstructors' <- mapM goConstructorDef _inductiveConstructors
  return
    InductiveDef
      { _inductiveName = goName _inductiveName,
        _inductiveConstructors = _inductiveConstructors'
      }

goConstructorDef ::
  Members '[Reader Mono.CompileInfoTable] r =>
  Mono.InductiveConstructorDef ->
  Sem r InductiveConstructorDef
goConstructorDef Mono.InductiveConstructorDef {..} = do
  _constructorParameters' <- mapM goType _constructorParameters
  return
    InductiveConstructorDef
      { _constructorName = goName _constructorName,
        _constructorParameters = _constructorParameters'
      }

goFunction :: Members '[Reader Mono.CompileInfoTable] r => Mono.Function -> Sem r Function
goFunction Mono.Function {..} = do
  _funLeft' <- goType _funLeft
  _funRight' <- goType _funRight
  return
    Function
      { _funLeft = _funLeft',
        _funRight = _funRight'
      }

goTypeIden :: Members '[Reader Mono.CompileInfoTable] r => Mono.TypeIden -> Sem r Type
goTypeIden = \case
  Mono.TypeIdenInductive n -> return (TypeIden (TypeIdenInductive (goName n)))
  Mono.TypeIdenAxiom n -> TypeVerbatim <$> goAxiomIden n

goType :: Members '[Reader Mono.CompileInfoTable] r => Mono.Type -> Sem r Type
goType = \case
  Mono.TypeIden t -> goTypeIden t
  Mono.TypeFunction f -> TypeFunction <$> goFunction f
  Mono.TypeUniverse -> error "MiniHaskell: universes in types not supported"
  Mono.TypeAny -> impossible
