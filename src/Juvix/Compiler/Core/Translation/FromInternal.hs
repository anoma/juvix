module Juvix.Compiler.Core.Translation.FromInternal where

import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty (fromList)
import Juvix.Compiler.Concrete.Data.Literal (LiteralLoc)
import Juvix.Compiler.Core.Data
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.BinderInfo
import Juvix.Compiler.Core.Info.LocationInfo
import Juvix.Compiler.Core.Info.NameInfo
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Transformation.Eta (etaExpandApps)
import Juvix.Compiler.Core.Translation.FromInternal.Data
import Juvix.Compiler.Core.Translation.FromSource (freshName)
import Juvix.Compiler.Internal.Extra qualified as Internal
import Juvix.Compiler.Internal.Translation.Extra qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking qualified as InternalTyped
import Juvix.Extra.Strings qualified as Str
import Safe (headMay)

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
          runReader (Internal.buildTable [m]) (runNameIdGen (registerFunctionDefs m))

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
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable, NameIdGen] r =>
  Internal.Module ->
  Sem r ()
registerFunctionDefs m = registerFunctionDefsBody (m ^. Internal.moduleBody)

registerFunctionDefsBody ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable, NameIdGen] r =>
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
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable, NameIdGen] r =>
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
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable, NameIdGen] r =>
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

      body <-
        if
            | null patterns -> goExpression 0 HashMap.empty (head (f ^. Internal.funDefClauses) ^. Internal.clauseBody)
            | otherwise -> do
                let vars :: HashMap Text Index = HashMap.fromList [(pack (show i), i) | i <- vs]
                let values = mkVar Info.empty <$> vs
                ms <- mapM (goFunctionClause' (length patterns) vars) (f ^. Internal.funDefClauses)
                let match = mkMatch' (fromList values) (toList ms)
                lamArgs' :: [Info] <- lamArgs
                return $ foldr mkLambda match lamArgs'
      registerIdentNode sym body
  where
    patterns :: [Internal.PatternArg]
    patterns = filter (\p -> p ^. Internal.patternArgIsImplicit == Internal.Explicit) (head (f ^. Internal.funDefClauses) ^. Internal.clausePatterns)

    vs :: [Index]
    vs = take (length patterns) [0 ..]

    mkName :: Text -> Sem r Name
    mkName txt = freshName KNameLocal txt (f ^. Internal.funDefName . Internal.nameLoc)

    lamArgs :: Sem r [Info]
    lamArgs = do
      ns <- mapM mkName (pack . show <$> vs)
      return $ binderNameInfo <$> ns

binderNameInfo :: Name -> Info
binderNameInfo name =
  Info.singleton (BinderInfo (Info.singleton (NameInfo name)))

fromPattern ::
  forall r.
  Members '[InfoTableBuilder, Reader Internal.InfoTable, State SucState, State SucTable, State SucTable] r =>
  Internal.Pattern ->
  Sem r Pattern
fromPattern = \case
  Internal.PatternWildcard {} -> return wildcard
  Internal.PatternVariable n -> do
    s <- gets (^. sucStateSucs) -- (^. sucTableTable . at (n ^. Internal.nameText))
    modify (over sucTableTable (HashMap.insert (n ^. Internal.nameText) s))
    return $ PatBinder (PatternBinder (setInfoName n Info.empty) wildcard)
  Internal.PatternConstructorApp c -> do
    let n = c ^. Internal.constrAppConstructor
    ctorInfo <- HashMap.lookupDefault impossible n <$> asks (^. Internal.infoConstructors)
    case ctorInfo ^. Internal.constructorInfoBuiltin of
      Just Internal.BuiltinNaturalZero -> return $ PatConst (PatternConst (setInfoName n Info.empty) (ConstInteger 0))
      Just Internal.BuiltinNaturalSuc -> do
        modify (over sucStateSucs (+ 1))
        fromPattern (fromJust (headMay (c ^. Internal.constrAppParameters)) ^. Internal.patternArgPattern)
      _ -> do
        tag <- ctorTag c
        args <- mapM (evalState emptySucState . fromPattern) ((^. Internal.patternArgPattern) <$> filter (\p -> p ^. Internal.patternArgIsImplicit == Explicit) (c ^. Internal.constrAppParameters))
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

goFunctionClause' ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  Internal.FunctionClause ->
  Sem r MatchBranch
goFunctionClause' varsNum vars clause = do
  (sucTable, pats) <- patterns
  let sucTable' = HashMap.filter (> 0) (sucTable ^. sucTableTable)
      pis = concatMap (reverse . getBinderPatternInfos) pats
      (vars', varsNum') =
        foldl'
          ( \(vs, k) name ->
              (HashMap.insert (name ^. nameText) k vs, k + 1)
          )
          (vars, varsNum)
          (map (fromJust . getInfoName) pis)
      letSpecs = mkLetSpecs vars' sucTable'
      vars'' = HashMap.union (letVars varsNum' letSpecs) vars'

  body <- goExpression (varsNum' + length sucTable') vars'' (clause ^. Internal.clauseBody)
  return $ MatchBranch Info.empty (fromList pats) (letNode letSpecs body)
  where
    patterns :: Sem r (SucTable, [Pattern])
    patterns =
      first (over sucTableTable (HashMap.filter (> 0)))
        <$> runState
          emptySucTable
          (reverse <$> mapM (evalState emptySucState . fromPattern) ps)

    ps :: [Internal.Pattern]
    ps = (^. Internal.patternArgPattern) <$> filter argIsImplicit (clause ^. Internal.clausePatterns)

    argIsImplicit :: Internal.PatternArg -> Bool
    argIsImplicit = (== Internal.Explicit) . (^. Internal.patternArgIsImplicit)

    letNode :: [LetSpec] -> Node -> Node
    letNode specs body = foldr f body specs
      where
        f :: LetSpec -> Node -> Node
        f s b = mkLet Info.empty (subN (s ^. letSpecIndex) (s ^. letSpecSucs)) b

    letVars :: Index -> [LetSpec] -> HashMap Text Index
    letVars num specs = HashMap.fromList (f <$> zip specs [0 ..])
      where
        f :: (LetSpec, Index) -> (Text, Index)
        f (s, i) = (s ^. letSpecBinder, num + i)

    mkLetSpecs :: HashMap Text Index -> HashMap Text Integer -> [LetSpec]
    mkLetSpecs vs sucTable = f <$> zip [0 ..] (HashMap.toList sucTable)
      where
        f :: (Int, (Text, Integer)) -> LetSpec
        f (i, (t, sucs)) = LetSpec t sucs (fromJust (HashMap.lookup t vs) + i)

    subN :: Index -> Integer -> Node
    subN idx n = mkBuiltinApp' OpIntSub [mkVar Info.empty idx, mkConstant Info.empty (ConstInteger n)]

goExpression ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable] r =>
  Index ->
  HashMap Text Index ->
  Internal.Expression ->
  Sem r Node
goExpression varsNum vars e = do
  node <- goExpression' varsNum vars e
  tab <- getInfoTable
  return $ etaExpandApps tab node

goExpression' ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable] r =>
  Index ->
  HashMap Text Index ->
  Internal.Expression ->
  Sem r Node
goExpression' varsNum vars = \case
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
