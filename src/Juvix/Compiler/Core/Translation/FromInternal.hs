module Juvix.Compiler.Core.Translation.FromInternal where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Concrete.Data.Literal (LiteralLoc)
import Juvix.Compiler.Core.Data
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.BinderInfo
import Juvix.Compiler.Core.Info.LocationInfo
import Juvix.Compiler.Core.Info.NameInfo
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Translation.FromInternal.Data
import Juvix.Compiler.Internal.Extra qualified as Internal
import Juvix.Compiler.Internal.Translation.Extra qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking qualified as InternalTyped
import Juvix.Extra.Strings qualified as Str

unsupported :: Text -> a
unsupported thing = error ("Internal to Core: Not yet supported: " <> thing)

fromInternal :: Internal.InternalTypedResult -> Sem k CoreResult
fromInternal i = do
  CoreResult . fst <$> runInfoTableBuilder emptyInfoTable (runReader (i ^. InternalTyped.resultIdenTypes) f)
  where
    f :: forall r. Members '[InfoTableBuilder, Reader InternalTyped.TypesTable] r => Sem r ()
    f = mapM_ coreModule (toList (i ^. InternalTyped.resultModules))
      where
        coreModule :: Internal.Module -> Sem r ()
        coreModule m = do
          registerInductiveDefs m
          runReader (Internal.buildTable [m]) (registerFunctionDefs m)

registerInductiveDefs ::
  forall r.
  Members '[InfoTableBuilder] r =>
  Internal.Module ->
  Sem r ()
registerInductiveDefs m = registerInductiveDefsBody (m ^. Internal.moduleBody)

registerInductiveDefsBody ::
  forall r.
  Members '[InfoTableBuilder] r =>
  Internal.ModuleBody ->
  Sem r ()
registerInductiveDefsBody body = mapM_ go (body ^. Internal.moduleStatements)
  where
    go :: Internal.Statement -> Sem r ()
    go = \case
      Internal.StatementInductive d -> goInductiveDef d
      Internal.StatementAxiom {} -> return ()
      Internal.StatementForeign {} -> return ()
      Internal.StatementFunction {} -> return ()
      Internal.StatementInclude i ->
        mapM_ go (i ^. Internal.includeModule . Internal.moduleBody . Internal.moduleStatements)

registerFunctionDefs ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable] r =>
  Internal.Module ->
  Sem r ()
registerFunctionDefs m = registerFunctionDefsBody (m ^. Internal.moduleBody)

registerFunctionDefsBody ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable] r =>
  Internal.ModuleBody ->
  Sem r ()
registerFunctionDefsBody body = mapM_ go (body ^. Internal.moduleStatements)
  where
    go :: Internal.Statement -> Sem r ()
    go = \case
      Internal.StatementFunction f -> goMutualBlock f
      Internal.StatementInclude i -> mapM_ go (i ^. Internal.includeModule . Internal.moduleBody . Internal.moduleStatements)
      _ -> return ()

goMutualBlock ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable] r =>
  Internal.MutualBlock ->
  Sem r ()
goMutualBlock m = mapM_ goFunctionDef (m ^. Internal.mutualFunctions)

goInductiveDef ::
  forall r.
  Members '[InfoTableBuilder] r =>
  Internal.InductiveDef ->
  Sem r ()
goInductiveDef i = do
  sym <- freshSymbol
  ctorInfos <- mapM (goConstructor sym) (i ^. Internal.inductiveConstructors)
  unless (isJust (i ^. Internal.inductiveBuiltin)) $ do
    let info =
          InductiveInfo
            { _inductiveName = i ^. Internal.inductiveName,
              _inductiveSymbol = sym,
              _inductiveKind = mkDynamic',
              _inductiveConstructors = ctorInfos,
              _inductiveParams = [],
              _inductivePositive = i ^. Internal.inductivePositive
            }
    registerInductive info

goConstructor ::
  forall r.
  Members '[InfoTableBuilder] r =>
  Symbol ->
  Internal.InductiveConstructorDef ->
  Sem r ConstructorInfo
goConstructor sym ctor = do
  tag <- freshTag
  let info =
        ConstructorInfo
          { _constructorName = ctor ^. Internal.inductiveConstructorName,
            _constructorTag = tag,
            _constructorType = mkDynamic',
            _constructorArgsNum = length (ctor ^. Internal.inductiveConstructorParameters),
            _constructorInductive = sym
          }
  registerConstructor info
  return info

goFunctionDef ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable] r =>
  Internal.FunctionDef ->
  Sem r ()
goFunctionDef f
  | isJust (f ^. Internal.funDefBuiltin) = return ()
  | otherwise = do
      sym <- freshSymbol
      let info =
            IdentifierInfo
              { _identifierName = Just (f ^. Internal.funDefName),
                _identifierSymbol = sym,
                _identifierType = mkDynamic',
                _identifierArgsNum = 0,
                _identifierArgsInfo = [],
                _identifierIsExported = False
              }
      registerIdent info
      when (f ^. Internal.funDefName . Internal.nameText == Str.main) (registerMain sym)
      mapM_ (goFunctionClause sym) (f ^. Internal.funDefClauses)

binderNameInfo :: Name -> Info
binderNameInfo name =
  Info.singleton (BinderInfo (Info.singleton (NameInfo name)))

fromPattern :: forall r. Members '[InfoTableBuilder] r => Internal.Pattern -> Sem r Pattern
fromPattern = \case
  Internal.PatternWildcard {} -> return wildcard
  Internal.PatternVariable n -> return $ PatBinder (PatternBinder (setInfoName n Info.empty) wildcard)
  Internal.PatternConstructorApp c -> do
    let n = c ^. Internal.constrAppConstructor
    tag <- ctorTag c
    args <- mapM fromPattern ((^. Internal.patternArgPattern) <$> filter (\p -> p ^. Internal.patternArgIsImplicit == Explicit) (c ^. Internal.constrAppParameters))
    return $ PatConstr (PatternConstr (setInfoName n Info.empty) tag args)
  where
    wildcard :: Pattern
    wildcard = PatWildcard (PatternWildcard Info.empty)

    ctorTag :: Internal.ConstructorApp -> Sem r Tag
    ctorTag c = do
      let txt = c ^. Internal.constrAppConstructor . Internal.nameText
      i <- getIdent txt
      return $ case i of
        Just (IdentTag tag) -> tag
        Just (IdentSym {}) -> error ("internal to core: not a constructor " <> txt)
        Nothing -> error ("internal to core: undeclared identifier: " <> txt)

goFunctionClause ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable] r =>
  Symbol ->
  Internal.FunctionClause ->
  Sem r ()
goFunctionClause sym clause = do
  body <- goExpression (length args) vars (clause ^. Internal.clauseBody)
  registerIdentNode sym (foldr mkLambda body lamArgs)
  where
    args :: [Internal.Pattern]
    args = (^. Internal.patternArgPattern) <$> filter (\p -> p ^. Internal.patternArgIsImplicit == Internal.Explicit) (clause ^. Internal.clausePatterns)

    vars :: HashMap Text Index
    vars = HashMap.fromList $ do
      a <- zip [0 ..] args
      case a of
        (varNum, Internal.PatternVariable n) -> [(n ^. Internal.nameText, varNum)]
        (_, Internal.PatternConstructorApp {}) -> []
        (_, Internal.PatternWildcard Wildcard {}) -> []

    lamArgs :: [Info]
    lamArgs = toBinderInfo <$> args
      where
        toBinderInfo :: Internal.Pattern -> Info
        toBinderInfo = \case
          Internal.PatternVariable n -> binderNameInfo n
          _ -> Info.empty

goExpression ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable] r =>
  Index ->
  HashMap Text Index ->
  Internal.Expression ->
  Sem r Node
goExpression varsNum vars = \case
  Internal.ExpressionLiteral l -> return (goLiteral l)
  Internal.ExpressionIden i -> case i of
    Internal.IdenVar n -> do
      let k = HashMap.lookupDefault impossible txt vars
      return (mkVar (Info.singleton (NameInfo n)) (varsNum - k - 1))
    Internal.IdenFunction n -> do
      m <- getIdent txt
      return $ case m of
        Just (IdentSym sym) -> mkIdent (Info.singleton (NameInfo n)) sym
        Just (IdentTag {}) -> error ("internal to core: not a function: " <> txt)
        Nothing -> error ("internal to core: undeclared identifier: " <> txt)
    Internal.IdenInductive {} -> error "goExpression inductive"
    Internal.IdenConstructor n -> do
      ctorInfo <- HashMap.lookupDefault impossible n <$> asks (^. Internal.infoConstructors)
      case ctorInfo ^. Internal.constructorInfoBuiltin of
        Just Internal.BuiltinNaturalZero -> return $ mkConstant (Info.singleton (LocationInfo (getLoc n))) (ConstInteger 0)
        _ -> do
          m <- getIdent txt
          return $ case m of
            Just (IdentTag tag) -> mkConstr (Info.singleton (NameInfo n)) tag []
            Just (IdentSym {}) -> error ("internal to core: not a constructor " <> txt)
            Nothing -> error ("internal to core: undeclared identifier: " <> txt)
    Internal.IdenAxiom {} -> error ("goExpression axiom: " <> txt)
    where
      txt :: Text
      txt = Internal.getName i ^. Internal.nameText
  Internal.ExpressionApplication a -> goApplication varsNum vars a
  x -> unsupported ("goExpression: " <> show (getLoc x))

goApplication ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable] r =>
  Index ->
  HashMap Text Index ->
  Internal.Application ->
  Sem r Node
goApplication varsNum vars a = do
  (f, args) <- Internal.unfoldPolyApplication a
  let exprArgs :: Sem r [Node]
      exprArgs = mapM (goExpression varsNum vars) args
      app :: Sem r Node
      app = do
        fExpr <- goExpression varsNum vars f
        mkApps' fExpr <$> exprArgs
  case f of
    Internal.ExpressionIden (Internal.IdenConstructor n) -> do
      ctorInfo <- HashMap.lookupDefault impossible n <$> asks (^. Internal.infoConstructors)
      case ctorInfo ^. Internal.constructorInfoBuiltin of
        Just Internal.BuiltinNaturalSuc -> do
          as <- exprArgs
          return $ mkBuiltinApp' OpIntAdd (mkConstant Info.empty (ConstInteger 1) : as)
        _ -> app
    _ -> app

goLiteral :: LiteralLoc -> Node
goLiteral l = case l ^. withLocParam of
  Internal.LitString s -> mkConstant (Info.singleton (LocationInfo (l ^. withLocInt))) (ConstString s)
  Internal.LitInteger i -> mkConstant (Info.singleton (LocationInfo (l ^. withLocInt))) (ConstInteger i)
