module MiniJuvix.Translation.MicroJuvixToMiniHaskell where
import MiniJuvix.Syntax.MicroJuvix.Language
import MiniJuvix.Prelude
import qualified MiniJuvix.Syntax.MiniHaskell.Language as H
import MiniJuvix.Syntax.MicroJuvix.InfoTable
import MiniJuvix.Syntax.ForeignBlock
import MiniJuvix.Syntax.Backends
import MiniJuvix.Syntax.NameId
import Prettyprinter
import qualified Data.Text as Text

translateModule :: Module -> Either Err H.Module
translateModule m = run (runError (runReader table (goModule m)))
  where
  table = buildTable m

type Err = Text

goModule :: Members '[Error Err, Reader InfoTable] r => Module -> Sem r H.Module
goModule Module {..} = do
  _moduleBody' <- goModuleBody _moduleBody
  return H.Module {
    _moduleName = goName (_moduleName),
    _moduleBody = _moduleBody'
    }

unsupported :: Text -> a
unsupported msg = error $ msg <> " not yet supported"

goModuleBody :: Members '[Error Err, Reader InfoTable] r =>
  ModuleBody -> Sem r H.ModuleBody
goModuleBody ModuleBody {..} =
  H.ModuleBody <$> mapMaybeM goStatement _moduleStatements

goStatement :: Members '[Error Err, Reader InfoTable] r => Statement -> Sem r (Maybe H.Statement)
goStatement = \case
  StatementInductive d -> Just . H.StatementInductive <$> goInductive d
  StatementFunction d -> Just . H.StatementFunction <$> goFunctionDef d
  StatementForeign d -> return (goForeign d)
  StatementAxiom {} -> return Nothing

goForeign :: ForeignBlock -> Maybe H.Statement
goForeign b = case b ^. foreignBackend of
  BackendGhc -> Just (H.StatementVerbatim (b ^. foreignCode))
  _ -> Nothing

lookupAxiom :: Members '[Error Err, Reader InfoTable] r => Name -> Sem r AxiomInfo
lookupAxiom n =
  fromMaybe impossible . (^. infoAxioms . at n) <$> ask

goIden :: Members '[Error Err, Reader InfoTable] r => Iden -> Sem r H.Expression
goIden = \case
  IdenFunction fun -> return (goName' fun)
  IdenConstructor c -> return (goName' c)
  IdenVar v -> return (goName' v)
  IdenAxiom a -> H.ExpressionVerbatim <$> goAxiomIden a

throwErr :: Member (Error Err) r => Text -> Sem r a
throwErr = throw

goAxiomIden :: Members '[Error Err, Reader InfoTable] r => Name -> Sem r Text
goAxiomIden n = do
  backends <- (^. axiomInfoBackends) <$> lookupAxiom n
  case firstJust getCode backends of
    Nothing -> throwErr ("ghc does not support this primitive:" <> show (pretty n))
    Just t -> return t
  where
  getCode :: BackendItem -> Maybe Text
  getCode b =
    guard (BackendGhc == b ^. backendItemBackend)
    $> b ^. backendItemCode

goName' :: Name -> H.Expression
goName' = H.ExpressionIden . goName

goName :: Name -> H.Name
goName n = H.Name {
  _nameText = goNameText n,
  _nameKind = n ^. nameKind
  }

goNameText :: Name -> Text
goNameText n =
  adaptFirstLetter lexeme <> "_" <> show (n ^. nameId . unNameId)
  where
  lexeme
    | Text.null lexeme' = "v"
    | otherwise = lexeme'
    where
    lexeme' = Text.filter isValidChar (n ^. nameText)
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
    capital = case n ^. nameKind of
      KNameConstructor -> True
      KNameInductive -> True
      KNameTopModule -> True
      KNameLocalModule -> True
      _ -> False

goFunctionDef :: Members '[Error Err, Reader InfoTable] r => FunctionDef -> Sem r H.FunctionDef
goFunctionDef FunctionDef {..} = do
  _funDefType' <- goType _funDefType
  _funDefClauses' <- mapM goFunctionClause _funDefClauses
  return H.FunctionDef {
    _funDefName = goName _funDefName,
    _funDefType = _funDefType',
    _funDefClauses = _funDefClauses'
    }

goPattern :: Pattern -> H.Pattern
goPattern = \case
  PatternVariable v -> H.PatternVariable (goName v)
  PatternConstructorApp a -> H.PatternConstructorApp (goConstructorApp a)
  PatternWildcard -> H.PatternWildcard

goConstructorApp :: ConstructorApp -> H.ConstructorApp
goConstructorApp c = H.ConstructorApp {
  _constrAppConstructor = goName (c ^. constrAppConstructor),
  _constrAppParameters = map goPattern (c ^. constrAppParameters)
  }

goExpression :: Members '[Error Err, Reader InfoTable] r =>
  Expression -> Sem r H.Expression
goExpression = \case
  ExpressionIden i -> goIden i
  ExpressionTyped t -> goExpression (t ^. typedExpression)
  ExpressionApplication a -> H.ExpressionApplication <$> goApplication a
  ExpressionLiteral l -> return (H.ExpressionLiteral l)

goApplication :: Members '[Error Err, Reader InfoTable] r =>
  Application -> Sem r H.Application
goApplication Application {..} = do
  _appLeft' <- goExpression _appLeft
  _appRight' <- goExpression _appRight
  return H.Application {
    _appLeft = _appLeft',
    _appRight = _appRight'
    }

goFunctionClause :: Members '[Error Err, Reader InfoTable] r =>
 FunctionClause -> Sem r H.FunctionClause
goFunctionClause FunctionClause {..} = do
  _clauseBody' <- goExpression _clauseBody
  let _clausePatterns' = map goPattern _clausePatterns
  return H.FunctionClause {
    _clauseBody = _clauseBody',
    _clausePatterns = _clausePatterns'
    }

goInductive :: Members '[Error Err, Reader InfoTable] r =>
  InductiveDef -> Sem r H.InductiveDef
goInductive InductiveDef {..} = do
  _inductiveConstructors' <- mapM goConstructorDef _inductiveConstructors
  return H.InductiveDef {
    _inductiveName = goName _inductiveName,
    _inductiveConstructors = _inductiveConstructors'
    }

goConstructorDef :: Members '[Error Err, Reader InfoTable] r =>
  InductiveConstructorDef -> Sem r H.InductiveConstructorDef
goConstructorDef InductiveConstructorDef {..} = do
  _constructorParameters' <- mapM goType _constructorParameters
  return H.InductiveConstructorDef {
    _constructorName = goName _constructorName,
    _constructorParameters = _constructorParameters'
    }

goFunction :: Members '[Error Err, Reader InfoTable] r => Function -> Sem r H.Function
goFunction Function {..} = do
  _funLeft' <- goType _funLeft
  _funRight' <- goType _funRight
  return H.Function {
    _funLeft = _funLeft',
    _funRight = _funRight'
    }

goTypeIden :: Members '[Error Err, Reader InfoTable] r => TypeIden -> Sem r H.Type
goTypeIden = \case
  TypeIdenInductive n -> return (H.TypeIden (H.TypeIdenInductive (goName n)))
  TypeIdenAxiom n -> H.TypeVerbatim <$> goAxiomIden n

goType :: Members '[Error Err, Reader InfoTable] r => Type -> Sem r H.Type
goType = \case
  TypeIden t -> goTypeIden t
  TypeFunction f -> H.TypeFunction <$> goFunction f
  TypeUniverse -> throwErr "MiniHaskell: universes in types not supported"
  TypeAny -> impossible
